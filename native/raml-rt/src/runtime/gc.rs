/// Garbage Collector - Automatic Memory Management
///
/// This module implements a generational garbage collector, which is an automatic
/// memory management system that reclaims memory from objects that are no longer
/// in use by the program.
///
/// # Why Two Generations?
///
/// The collector uses TWO separate memory regions (called "generations"):
/// - **Young Generation (Minor Heap)**: Where new objects are allocated. Most objects
///   die young (used briefly then discarded), so we collect this frequently.
/// - **Old Generation (Major Heap)**: Where long-lived objects are promoted. We collect
///   this less frequently because it's more expensive.
///
/// This is called the "generational hypothesis": most objects die young, so by separating
/// young and old objects, we can collect the young generation quickly and frequently.
///
/// # How Collection Works
///
/// **Minor Collection (Young Generation)**:
/// 1. Start from "roots" (global variables, stack, registers)
/// 2. Find all young objects that are still reachable
/// 3. Copy those objects to the old generation (called "promotion")
/// 4. Throw away everything left in the young generation
/// 5. This is FAST because most young objects are garbage
///
/// **Major Collection (Old Generation)**:
/// 1. Mark phase: Starting from roots, mark all reachable objects
/// 2. Sweep phase: Walk through old generation and free unmarked objects
/// 3. This is SLOW but happens less frequently
///
/// # The Remembered Set
///
/// Problem: What if an old object points to a young object? During minor GC,
/// we'd miss it because we don't scan all old objects!
///
/// Solution: The "remembered set" tracks which old objects point to young objects.
/// During minor GC, we treat these old objects as additional roots.
///
/// Maintained by "write barriers" - whenever we store a pointer, we check if it's
/// an old→young pointer and record it.

use crate::value::{Block, GcColor, Value};
use super::Result;
use std::collections::HashSet;

pub struct GarbageCollector {
    /// Remembered Set: Tracks old-generation objects that point to young-generation objects.
    /// When we do a minor GC, we need to scan these old objects as roots to find
    /// young objects that are still referenced.
    remembered_set: HashSet<*mut Block>,
    
    /// Mark Stack: Used during major GC's mark phase. We push objects here when we
    /// discover them, then pop and scan their children. This implements a depth-first
    /// traversal of the object graph.
    mark_stack: Vec<*mut Block>,
    
    /// Statistics about GC activity (how many collections, how much memory moved, etc.)
    pub stats: GcStats,
}

impl GarbageCollector {
    pub fn new() -> Self {
        GarbageCollector {
            remembered_set: HashSet::new(),
            mark_stack: Vec::new(),
            stats: GcStats::default(),
        }
    }
    
    /// Minor GC: Collect the Young Generation
    ///
    /// This is called when we run out of space in the young generation (minor heap).
    /// It's a "copying collector" - we copy live objects to the old generation and
    /// then throw away everything left behind.
    ///
    /// # Algorithm (Cheney's Algorithm)
    ///
    /// 1. Start with roots (values the program can currently access):
    ///    - Accumulator register
    ///    - Environment register  
    ///    - Stack
    ///    - Global variables
    ///    - Exception handlers
    ///    - Effect handler continuations
    ///
    /// 2. For each root that points to a young object:
    ///    - Copy (promote) that object to the old generation
    ///    - Update the root to point to the new location
    ///    - Leave a "forwarding pointer" in the old location
    ///
    /// 3. Scan objects we just promoted - if they point to young objects,
    ///    promote those too (recursively until done)
    ///
    /// 4. Also scan the "remembered set" - old objects that point to young objects
    ///
    /// 5. Reset the young generation's allocation pointer (all objects there are now garbage)
    ///
    /// # Parameters
    /// - `minor_heap`: The young generation where new allocations happen
    /// - `major_heap`: The old generation where promoted objects go
    /// - `roots`: All values currently accessible by the program
    pub fn minor_gc(
        &mut self,
        minor_heap: &mut super::memory::MinorHeap,
        major_heap: &mut super::memory::MajorHeap,
        roots: &[Value],
    ) -> Result<()> {
        self.stats.minor_collections += 1;
        
        // The "worklist" contains pointers to Value fields that might point to young objects.
        // We process this list, and when we find young objects, we:
        // 1. Promote them to the old generation
        // 2. Update the pointer to point to the new location
        // 3. Add the promoted object's fields to the worklist (to find more young objects)
        let mut worklist: Vec<*mut Value> = Vec::new();
        
        // Phase 1: Add roots that point to young objects
        // We need pointers to the Value slots (not the values themselves) so we can update them
        for &root in roots {
            if root.is_block() {
                if let Some(block_ptr) = root.as_block() {
                    // Only care about young objects - old objects stay where they are
                    if minor_heap.contains(block_ptr) {
                        worklist.push(root.as_raw() as *mut usize as *mut Value);
                    }
                }
            }
        }
        
        // Phase 2: Add fields from the remembered set
        // The remembered set contains old objects that point to young objects.
        // We need to scan their fields to find which young objects they reference.
        for &old_block in &self.remembered_set {
            let block = unsafe { &*old_block };
            // Only scan if the block contains pointers (not raw data like strings)
            if block.should_scan() {
                for i in 0..block.size() {
                    let field_ptr = unsafe {
                        // Point to the i-th field of the block (+1 to skip header)
                        (old_block as *mut Value).add(1 + i)
                    };
                    worklist.push(field_ptr);
                }
            }
        }
        
        // Phase 3: Process the worklist
        // This implements a copying collector - we copy live young objects to the old generation
        while let Some(value_ptr) = worklist.pop() {
            let value = unsafe { *value_ptr };
            
            // Skip non-pointer values (integers, booleans, etc.)
            if !value.is_block() {
                continue;
            }
            
            if let Some(block_ptr) = value.as_block() {
                // Skip old objects - they're already promoted
                if !minor_heap.contains(block_ptr) {
                    continue;
                }
                
                let block = unsafe { &*block_ptr };
                
                // Skip objects we've already promoted (marked black as a forwarding indicator)
                // In a real implementation, we'd store the forwarding address in the old object
                if block.color() == GcColor::Black {
                    continue;
                }
                
                // Copy the object to the old generation
                let new_block = self.promote_to_major(block_ptr, major_heap)?;
                
                unsafe {
                    // Mark the old copy as forwarded (black = already processed)
                    (*block_ptr).header().set_color(GcColor::Black);
                    
                    // Update the pointer that led us here to point to the new location
                    *value_ptr = Value::from_block_ptr(new_block);
                }
                
                // Add the promoted object's fields to the worklist
                // This ensures we recursively promote everything it points to
                let promoted = unsafe { &*new_block };
                if promoted.should_scan() {
                    for i in 0..promoted.size() {
                        let field_ptr = unsafe {
                            (new_block as *mut Value).add(1 + i)
                        };
                        worklist.push(field_ptr);
                    }
                }
            }
        }
        
        // Phase 4: Reset the young generation
        // All objects still in the young generation are garbage (unreachable)
        // We can throw them all away by resetting the allocation pointer
        minor_heap.reset();
        
        // Clear the remembered set - all those old→young pointers are gone now
        // (young objects were either promoted or collected as garbage)
        self.remembered_set.clear();
        
        Ok(())
    }
    
    /// Promote (copy) a young object to the old generation
    ///
    /// When we find a young object that's still alive, we need to move it to
    /// the old generation so it survives this collection.
    ///
    /// This involves:
    /// 1. Allocate space in the old generation
    /// 2. Copy the header and all fields
    /// 3. Update statistics
    ///
    /// # Parameters
    /// - `young_ptr`: Pointer to the object in the young generation
    /// - `major_heap`: The old generation where we'll copy it
    ///
    /// # Returns
    /// Pointer to the new copy in the old generation
    fn promote_to_major(
        &mut self,
        young_ptr: *const Block,
        major_heap: &mut super::memory::MajorHeap,
    ) -> Result<*mut Block> {
        let block = unsafe { &*young_ptr };
        let size = block.size();  // Number of fields
        let tag = block.tag();    // What kind of object (tuple, closure, etc.)
        
        // Allocate space in the old generation
        let new_block = major_heap.alloc(size, tag)?;
        
        // Copy all fields from young to old
        unsafe {
            for i in 0..size {
                let field = (*young_ptr).field(i);
                (*new_block).set_field(i, field);
            }
        }
        
        // Track how much we've promoted (for statistics/tuning)
        self.stats.promoted_words += size + 1;  // +1 for the header word
        
        Ok(new_block)
    }
    
    /// Major GC: Collect the Old Generation
    ///
    /// This is called when the old generation gets too full. It's more expensive
    /// than minor GC because we have to scan ALL old objects.
    ///
    /// Uses a "mark-sweep" algorithm:
    /// 1. **Mark Phase**: Starting from roots, mark all reachable objects
    /// 2. **Sweep Phase**: Walk through memory, free unmarked objects
    ///
    /// # Why Mark-Sweep instead of Copying?
    ///
    /// The young generation uses copying (fast, simple), but for the old generation
    /// we use mark-sweep because:
    /// - Old objects are long-lived, so most survive each collection
    /// - Copying all survivors would be expensive
    /// - Mark-sweep just updates a flag (mark) and frees garbage (sweep)
    ///
    /// # Parameters
    /// - `major_heap`: The old generation to collect
    /// - `roots`: All values currently accessible by the program
    pub fn major_gc(&mut self, major_heap: &mut super::memory::MajorHeap, roots: &[Value]) -> Result<()> {
        self.stats.major_collections += 1;
        
        // Phase 1: Mark all reachable objects
        self.mark_phase(major_heap, roots)?;
        
        // Phase 2: Free all unmarked objects (garbage)
        self.sweep_phase(major_heap)?;
        
        Ok(())
    }
    
    /// Mark Phase: Find all reachable objects
    ///
    /// We use a "tri-color" marking scheme:
    /// - **White**: Not yet visited (might be garbage)
    /// - **Gray**: Visited but children not yet scanned (in mark_stack)
    /// - **Black**: Visited and all children scanned (definitely reachable)
    ///
    /// Algorithm:
    /// 1. Start with all objects white
    /// 2. Mark roots gray, push onto mark_stack
    /// 3. While mark_stack not empty:
    ///    - Pop a gray object
    ///    - Mark it black
    ///    - Mark its children gray, push them onto mark_stack
    /// 4. At end: black objects are reachable, white objects are garbage
    fn mark_phase(&mut self, major_heap: &mut super::memory::MajorHeap, roots: &[Value]) -> Result<()> {
        self.mark_stack.clear();
        
        // Start by marking all roots
        for &root in roots {
            self.mark_value(root);
        }
        
        // Process the mark stack - this implements a depth-first traversal
        // of the object graph starting from roots
        while let Some(block_ptr) = self.mark_stack.pop() {
            let block = unsafe { &*block_ptr };
            
            // If this object contains pointers, mark all the objects it points to
            if block.should_scan() {
                for i in 0..block.size() {
                    let field = unsafe { block.field(i) };
                    self.mark_value(field);  // Recursively mark children
                }
            }
            
            // This object is now fully processed - mark it black
            unsafe {
                (*block_ptr).header().set_color(GcColor::Black);
            }
        }
        
        Ok(())
    }
    
    /// Mark a single value
    ///
    /// If it's a pointer to a white (unvisited) object:
    /// 1. Mark it gray (visited but not yet scanned)
    /// 2. Push it onto the mark stack for later processing
    fn mark_value(&mut self, val: Value) {
        // Immediate values (integers, booleans) aren't heap-allocated, so skip them
        if !val.is_block() {
            return;
        }
        
        if let Some(block_ptr) = val.as_block() {
            let block = unsafe { &*block_ptr };
            
            // Only process white (unvisited) objects
            if block.color() == GcColor::White {
                unsafe {
                    // Mark gray = visited, needs scanning
                    (*(block_ptr as *mut Block)).header().set_color(GcColor::Gray);
                }
                // Add to the stack so we'll scan its children later
                self.mark_stack.push(block_ptr as *mut Block);
            }
        }
    }
    
    /// Sweep Phase: Free all unmarked objects
    ///
    /// After marking, we know:
    /// - Black objects are reachable (keep them)
    /// - White objects are garbage (free them)
    ///
    /// The sweep walks through the heap and:
    /// 1. Frees white objects
    /// 2. Resets black objects to white (for next collection)
    fn sweep_phase(&mut self, major_heap: &mut super::memory::MajorHeap) -> Result<()> {
        major_heap.sweep();
        Ok(())
    }
    
    /// Write Barrier: Track old→young pointers
    ///
    /// # The Problem Write Barriers Solve
    ///
    /// During minor GC, we need to find all young objects that are still reachable.
    /// We scan:
    /// - Roots (stack, registers, globals)
    /// - Young objects found from other young objects
    ///
    /// But what if an OLD object points to a young object? We'd miss it!
    /// We can't scan ALL old objects during every minor GC - that would be slow.
    ///
    /// # The Solution
    ///
    /// Whenever we store a pointer into an old object, we check:
    /// "Is this an old object pointing to a young object?"
    /// If YES, add the old object to the remembered set.
    ///
    /// During minor GC, we scan the remembered set as additional roots.
    ///
    /// # When to Call This
    ///
    /// Call `write_barrier` every time you:
    /// - Store a value into a block's field (SetField opcode)
    /// - Initialize a newly allocated block's fields
    ///
    /// # Parameters
    /// - `block`: The object being modified (might be old)
    /// - `value`: The value being stored (might be young)
    /// - `minor_heap`: To check if value points to a young object
    pub fn write_barrier(&mut self, block: *mut Block, value: Value, minor_heap: &super::memory::MinorHeap) {
        // Only care about pointer values (not integers, booleans, etc.)
        if value.is_block() {
            if let Some(val_block) = value.as_block() {
                // Check: is this value pointing to a young object?
                if minor_heap.contains(val_block) {
                    // Yes! Record this old object in the remembered set
                    // During next minor GC, we'll scan it to find this young object
                    self.remembered_set.insert(block);
                }
            }
        }
    }
}

/// GC Statistics - Track Garbage Collection Activity
///
/// These statistics help us understand GC performance and tune parameters.
///
/// # Fields
/// - `minor_collections`: How many times we've collected the young generation
/// - `major_collections`: How many times we've collected the old generation
/// - `promoted_words`: Total memory moved from young to old (in machine words)
/// - `allocated_words`: Total memory allocated (for tracking allocation rate)
/// - `freed_words`: Total memory freed (for tracking collection effectiveness)
#[derive(Default, Debug)]
pub struct GcStats {
    pub minor_collections: usize,
    pub major_collections: usize,
    pub promoted_words: usize,
    pub allocated_words: usize,
    pub freed_words: usize,
}
