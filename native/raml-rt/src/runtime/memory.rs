/// Memory Management - Generational Heap for OCaml Values
///
/// This module implements a two-generation heap for allocating and managing
/// OCaml values (blocks, closures, tuples, etc.).
///
/// # Memory Layout
///
/// ```text
/// ┌─────────────────────────────────────────────────────────────┐
/// │                          HEAP                                │
/// ├──────────────────────────┬──────────────────────────────────┤
/// │   Young Generation        │    Old Generation                │
/// │   (Minor Heap)           │    (Major Heap)                  │
/// │                          │                                  │
/// │  - Small (e.g. 1MB)      │  - Large (grows as needed)       │
/// │  - Fast bump allocation  │  - Free-list allocation          │
/// │  - Collected frequently  │  - Collected infrequently        │
/// │  - Objects allocated here│  - Objects promoted from young   │
/// └──────────────────────────┴──────────────────────────────────┘
/// ```
///
/// # Allocation Strategy
///
/// 1. **Try young generation first** (fast bump allocation)
/// 2. **If full, trigger minor GC** to free space
/// 3. **Try young generation again** after GC
/// 4. **If still no space, allocate in old generation** (rare for new objects)
///
/// # Why This Design?
///
/// Most objects die young (used briefly then discarded). By separating young and
/// old objects, we can:
/// - Collect the young generation quickly and frequently
/// - Collect the old generation slowly and infrequently
/// - Achieve good performance with minimal pause times

use crate::value::{Block, BlockHeader, GcColor, Value};
use super::{Result, Error};
use super::gc::GarbageCollector;
use std::alloc::{alloc, dealloc, Layout};

/// The Complete Heap - Young + Old Generations + GC
pub struct Heap {
    /// Young generation - where new objects are allocated
    minor: MinorHeap,
    
    /// Old generation - where long-lived objects are promoted
    major: MajorHeap,
    
    /// Garbage collector - manages collection of both generations
    gc: GarbageCollector,
}

impl Heap {
    /// Create a new heap with specified young generation size
    ///
    /// # Parameters
    /// - `minor_size_bytes`: Size of young generation (e.g., 1MB = 1024 * 1024)
    ///
    /// Typical sizes:
    /// - Small programs: 512KB - 1MB
    /// - Medium programs: 2MB - 8MB
    /// - Large programs: 16MB - 64MB
    pub fn new(minor_size_bytes: usize) -> Self {
        Heap {
            minor: MinorHeap::new(minor_size_bytes),
            major: MajorHeap::new(),
            gc: GarbageCollector::new(),
        }
    }
    
    /// Allocate a new block (object) on the heap
    ///
    /// This is the main allocation function. It tries to allocate in the young
    /// generation first (fast), and triggers GC if needed.
    ///
    /// # Algorithm
    ///
    /// 1. Try allocating in young generation (fast bump allocation)
    /// 2. If no space, trigger minor GC to free young objects
    /// 3. Try allocating in young generation again
    /// 4. If still no space, allocate directly in old generation (rare)
    ///
    /// # Parameters
    /// - `size`: Number of fields in the block (not including header)
    /// - `tag`: Type of block (tuple, closure, string, etc.)
    /// - `roots`: Current program roots (for GC if needed)
    ///
    /// # Returns
    /// Pointer to newly allocated block with uninitialized fields
    ///
    /// # Safety
    /// Caller must initialize all fields before next allocation (which might trigger GC)
    pub fn alloc_block(&mut self, size: usize, tag: u8, roots: &mut Vec<Value>) -> Result<*mut Block> {
        // Fast path: try young generation (most common case)
        if let Some(block) = self.minor.try_alloc(size, tag) {
            return Ok(block);
        }
        
        // Young generation is full - collect it
        self.minor_gc(roots)?;
        
        // Try again after GC freed some space
        if let Some(block) = self.minor.try_alloc(size, tag) {
            return Ok(block);
        }
        
        // Still no space (object too large or young gen too small)
        // Allocate directly in old generation
        self.major.alloc(size, tag)
    }
    
    /// Trigger a minor (young generation) garbage collection
    ///
    /// See `gc.rs` for detailed algorithm documentation.
    pub fn minor_gc(&mut self, roots: &[Value]) -> Result<()> {
        self.gc.minor_gc(&mut self.minor, &mut self.major, roots)
    }
    
    /// Trigger a major (old generation) garbage collection
    ///
    /// See `gc.rs` for detailed algorithm documentation.
    pub fn major_gc(&mut self, roots: &[Value]) -> Result<()> {
        self.gc.major_gc(&mut self.major, roots)
    }
    
    /// Record a pointer store for the write barrier
    ///
    /// Call this whenever storing a value into a block's field.
    /// See `gc.rs` for why this is necessary.
    pub fn write_barrier(&mut self, block: *mut Block, value: Value) {
        self.gc.write_barrier(block, value, &self.minor);
    }
    
    /// Get GC statistics
    pub fn stats(&self) -> &super::gc::GcStats {
        &self.gc.stats
    }
    
    /// Check if a value points to the young generation
    pub fn is_young(&self, val: Value) -> bool {
        if let Some(ptr) = val.as_block() {
            self.minor.contains(ptr)
        } else {
            false
        }
    }
}

/// Young Generation (Minor Heap) - Fast Bump Allocator
///
/// The young generation uses a simple "bump allocator":
/// - Start with a contiguous block of memory
/// - Keep a pointer to the next free spot
/// - To allocate: just move the pointer
/// - To collect: throw everything away and reset the pointer
///
/// # Memory Layout
///
/// ```text
/// ┌──────────────────────────────────────────────────┐
/// │              Young Generation                     │
/// ├──────────────────────────────────────────────────┤
/// │ base                                      end     │
/// │  ↓                                         ↓      │
/// │  [obj1][obj2][obj3][ free space ]                │
/// │                     ↑                             │
/// │                    ptr (next allocation)          │
/// └──────────────────────────────────────────────────┘
/// ```
///
/// # Why Allocate Backwards?
///
/// We allocate from high addresses downward (ptr starts at end, moves toward base).
/// This is an OCaml convention - it doesn't really matter which direction, but
/// backwards allocation can have cache benefits in some scenarios.
///
/// # Collection
///
/// When the young generation fills up:
/// 1. GC copies live objects to old generation
/// 2. Reset ptr = end (throw away everything)
/// 3. Continue allocating
pub struct MinorHeap {
    /// Start of the memory region
    base: *mut u8,
    
    /// Size in bytes
    size: usize,
    
    /// Current allocation pointer (moves from end toward base)
    ptr: *mut u8,
    
    /// End of the memory region (ptr starts here)
    end: *mut u8,
    
    /// Statistics tracking
    stats: MinorStats,
}

unsafe impl Send for MinorHeap {}
unsafe impl Sync for MinorHeap {}

impl MinorHeap {
    /// Create a new young generation
    ///
    /// Allocates a contiguous block of memory for bump allocation.
    ///
    /// # Panics
    /// Panics if allocation fails (out of system memory)
    pub fn new(size_bytes: usize) -> Self {
        // Align to 8 bytes for proper pointer alignment
        let layout = Layout::from_size_align(size_bytes, 8)
            .expect("Invalid heap layout");
        
        let base = unsafe { alloc(layout) };
        if base.is_null() {
            panic!("Failed to allocate minor heap");
        }
        
        let end = unsafe { base.add(size_bytes) };
        
        MinorHeap {
            base,
            size: size_bytes,
            ptr: end,  // Start at the end, allocate backwards
            end,
            stats: MinorStats::default(),
        }
    }
    
    /// Try to allocate a block in the young generation
    ///
    /// This is a "bump allocator" - just move the pointer.
    /// Super fast: no searching for free space, no fragmentation.
    ///
    /// # Algorithm
    ///
    /// 1. Calculate bytes needed (size_words + 1 header) * sizeof(word)
    /// 2. Move ptr backward by that amount
    /// 3. If we hit the base, return None (out of space)
    /// 4. Otherwise, write the header and return the pointer
    ///
    /// # Parameters
    /// - `size_words`: Number of fields (not including header)
    /// - `tag`: Type of block
    ///
    /// # Returns
    /// - `Some(ptr)`: Allocation succeeded
    /// - `None`: Out of space, need GC
    pub fn try_alloc(&mut self, size_words: usize, tag: u8) -> Option<*mut Block> {
        // Calculate space needed: (fields + header) * word_size
        let bytes_needed = (size_words + 1) * std::mem::size_of::<usize>();
        
        // Try to bump the pointer backward
        let new_ptr = unsafe { self.ptr.sub(bytes_needed) };
        
        // Check if we have enough space
        if new_ptr < self.base {
            return None;  // Out of space - caller should trigger GC
        }
        
        // Allocation succeeded - update pointer and stats
        self.ptr = new_ptr;
        self.stats.allocated_words += size_words + 1;
        self.stats.allocations += 1;
        
        // Initialize the block header
        let block = new_ptr as *mut Block;
        unsafe {
            let header = BlockHeader::new(size_words, tag, GcColor::White);
            std::ptr::write(block as *mut BlockHeader, header);
        }
        
        Some(block)
    }
    
    /// Reset the young generation (throw away all objects)
    ///
    /// Called after minor GC - live objects have been copied to old generation,
    /// so everything left is garbage. We can just reset the pointer to reclaim
    /// all the space.
    pub fn reset(&mut self) {
        self.ptr = self.end;
        self.stats.collections += 1;
    }
    
    pub fn contains(&self, ptr: *const Block) -> bool {
        let addr = ptr as usize;
        let base_addr = self.base as usize;
        let end_addr = self.end as usize;
        addr >= base_addr && addr < end_addr
    }
    
    pub fn stats(&self) -> &MinorStats {
        &self.stats
    }
}

impl Drop for MinorHeap {
    fn drop(&mut self) {
        let layout = Layout::from_size_align(self.size, 8)
            .expect("Invalid heap layout");
        unsafe {
            dealloc(self.base, layout);
        }
    }
}

#[derive(Default)]
pub struct MinorStats {
    pub allocated_words: usize,
    pub allocations: usize,
    pub collections: usize,
}

pub struct MajorHeap {
    pools: Vec<Pool>,
    free_lists: Vec<Vec<*mut Block>>,
    mark_stack: Vec<*mut Block>,
    stats: MajorStats,
}

impl MajorHeap {
    const POOL_SIZE: usize = 64 * 1024;
    const NUM_FREE_LISTS: usize = 32;
    
    pub fn new() -> Self {
        MajorHeap {
            pools: Vec::new(),
            free_lists: vec![Vec::new(); Self::NUM_FREE_LISTS],
            mark_stack: Vec::new(),
            stats: MajorStats::default(),
        }
    }
    
    pub fn alloc(&mut self, size_words: usize, tag: u8) -> Result<*mut Block> {
        let size_class = self.size_to_class(size_words);
        
        if let Some(&block) = self.free_lists[size_class].last() {
            self.free_lists[size_class].pop();
            unsafe {
                (*block).header().set_color(GcColor::White);
            }
            return Ok(block);
        }
        
        let pool = Pool::new(Self::POOL_SIZE)?;
        let block = pool.alloc(size_words, tag)?;
        self.pools.push(pool);
        self.stats.allocated_words += size_words + 1;
        
        Ok(block)
    }
    
    fn size_to_class(&self, size: usize) -> usize {
        if size >= Self::NUM_FREE_LISTS {
            Self::NUM_FREE_LISTS - 1
        } else {
            size
        }
    }
    
    pub fn mark_roots(&mut self, roots: &[Value]) {
        for &root in roots {
            self.mark(root);
        }
        
        self.mark_loop();
    }
    
    fn mark(&mut self, val: Value) {
        if let Some(block_ptr) = val.as_block() {
            let block = unsafe { &*block_ptr };
            
            if block.color() == GcColor::White {
                block.header().set_color(GcColor::Gray);
                self.mark_stack.push(block_ptr as *mut Block);
            }
        }
    }
    
    fn mark_loop(&mut self) {
        while let Some(block_ptr) = self.mark_stack.pop() {
            let block = unsafe { &*block_ptr };
            
            if block.should_scan() {
                for i in 0..block.size() {
                    let field = unsafe { block.field(i) };
                    self.mark(field);
                }
            }
            
            block.header().set_color(GcColor::Black);
        }
    }
    
    pub fn sweep(&mut self) {
        for pool in &mut self.pools {
            pool.sweep(&mut self.free_lists, Self::NUM_FREE_LISTS);
        }
        
        self.stats.collections += 1;
    }
}

#[derive(Default)]
pub struct MajorStats {
    pub allocated_words: usize,
    pub collections: usize,
}

struct Pool {
    base: *mut u8,
    size: usize,
    ptr: *mut u8,
}

unsafe impl Send for Pool {}
unsafe impl Sync for Pool {}

impl Pool {
    fn new(size: usize) -> Result<Self> {
        let layout = Layout::from_size_align(size, 8)
            .map_err(|_| Error::OutOfMemory)?;
        
        let base = unsafe { alloc(layout) };
        if base.is_null() {
            return Err(Error::OutOfMemory);
        }
        
        Ok(Pool {
            base,
            size,
            ptr: base,
        })
    }
    
    fn alloc(&self, size_words: usize, tag: u8) -> Result<*mut Block> {
        let bytes_needed = (size_words + 1) * std::mem::size_of::<usize>();
        
        let block_ptr = self.ptr;
        let new_ptr = unsafe { self.ptr.add(bytes_needed) };
        
        if new_ptr as usize > (self.base as usize + self.size) {
            return Err(Error::OutOfMemory);
        }
        
        let block = block_ptr as *mut Block;
        unsafe {
            let header = BlockHeader::new(size_words, tag, GcColor::White);
            std::ptr::write(block as *mut BlockHeader, header);
        }
        
        Ok(block)
    }
    
    fn sweep(&mut self, free_lists: &mut [Vec<*mut Block>], num_classes: usize) {
        let mut cursor = self.base;
        let end = unsafe { self.base.add(self.size) };
        
        while cursor < end {
            let block = cursor as *mut Block;
            let block_ref = unsafe { &*block };
            
            let size = block_ref.size();
            let bytes = (size + 1) * std::mem::size_of::<usize>();
            
            match block_ref.color() {
                GcColor::White => {
                    let size_class = if size >= num_classes { 
                        num_classes - 1 
                    } else { 
                        size 
                    };
                    free_lists[size_class].push(block);
                }
                GcColor::Black | GcColor::Gray => {
                    block_ref.header().set_color(GcColor::White);
                }
                GcColor::Immobile => {}
            }
            
            cursor = unsafe { cursor.add(bytes) };
        }
    }
}

impl Drop for Pool {
    fn drop(&mut self) {
        let layout = Layout::from_size_align(self.size, 8)
            .expect("Invalid pool layout");
        unsafe {
            dealloc(self.base, layout);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_minor_heap_alloc() {
        let mut heap = MinorHeap::new(1024 * 1024);
        
        let block = heap.try_alloc(10, Tag::CONS).expect("Allocation failed");
        assert!(!block.is_null());
        
        let block_ref = unsafe { &*block };
        assert_eq!(block_ref.size(), 10);
        assert_eq!(block_ref.tag(), Tag::CONS);
    }
    
    #[test]
    fn test_heap_alloc() {
        let mut heap = Heap::new(1024 * 1024);
        let mut roots = vec![];
        
        let block = heap.alloc_block(5, Tag::CONS, &mut roots).expect("Allocation failed");
        assert!(!block.is_null());
        
        let block_ref = unsafe { &*block };
        assert_eq!(block_ref.size(), 5);
    }
}
