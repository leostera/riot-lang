use crate::value::{Value, VAL_UNIT, Block};
use super::{Result, Error, Heap, LoadedBytecode, PrimitiveTable};

const STACK_SIZE: usize = 64 * 1024;

use super::fiber::{Continuation, EffectHandler, FiberPool};

pub struct Interpreter {
    pc: usize,
    accu: Value,
    env: Value,
    stack: Vec<Value>,
    extra_args: isize,
    global_data: Vec<Value>,
    primitives: PrimitiveTable,
    trap_sp: Option<usize>,
    current_handler: Option<Box<EffectHandler>>,
    fiber_pool: FiberPool,
    continuations: Vec<Continuation>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            pc: 0,
            accu: VAL_UNIT,
            env: VAL_UNIT,
            stack: Vec::with_capacity(STACK_SIZE),
            extra_args: 0,
            global_data: Vec::new(),
            primitives: PrimitiveTable::new(),
            trap_sp: None,
            current_handler: None,
            fiber_pool: FiberPool::default(),
            continuations: Vec::new(),
        }
    }
    
    /// Collect GC Roots - Find All Reachable Values
    ///
    /// "Roots" are values that the program can currently access directly.
    /// The GC starts from roots and follows pointers to find all reachable objects.
    ///
    /// # What Are Roots?
    ///
    /// In the OCaml bytecode interpreter, roots include:
    ///
    /// 1. **Accumulator** (`accu`): The primary working register
    /// 2. **Environment** (`env`): Current function's closure environment
    /// 3. **Stack**: All values on the evaluation stack
    /// 4. **Global Data**: Module-level values
    /// 5. **Exception Handlers**: Stored exception continuation environments
    /// 6. **Effect Handlers**: Continuations and handler closures
    ///
    /// # Why Collect Roots?
    ///
    /// Before GC runs, we need to tell it "these values are in use."
    /// The GC will then:
    /// 1. Mark these roots as reachable
    /// 2. Follow pointers from roots to find more reachable objects
    /// 3. Collect everything NOT reachable (garbage)
    ///
    /// # Important
    ///
    /// This clears the provided vector and fills it with current roots.
    pub fn collect_roots(&self, roots: &mut Vec<Value>) {
        roots.clear();
        
        // Register roots: the interpreter's main working values
        roots.push(self.accu);
        roots.push(self.env);
        
        // Stack roots: everything currently on the evaluation stack
        roots.extend_from_slice(&self.stack);
        
        // Global roots: module-level values that persist across calls
        roots.extend_from_slice(&self.global_data);
        
        // Continuation roots: saved interpreter states from effect handlers
        for cont in &self.continuations {
            roots.push(cont.accu);
            roots.push(cont.env);
            roots.extend_from_slice(&cont.stack);
        }
        
        // Effect handler roots: handler closures and their parent stacks
        let mut handler_opt = self.current_handler.as_deref();
        while let Some(handler) = handler_opt {
            if let Some(parent_stack) = &handler.parent_stack {
                roots.extend_from_slice(parent_stack);
            }
            roots.push(handler.handler_closure);
            handler_opt = handler.parent_handler.as_deref();
        }
    }
    
    /// Allocate a block with automatic root collection
    ///
    /// This is a convenience wrapper around `heap.alloc_block` that automatically
    /// collects roots from the interpreter state.
    ///
    /// # Why This Helper Exists
    ///
    /// Every allocation might trigger GC, and GC needs to know what values are
    /// in use (roots). Rather than manually collecting roots at every allocation
    /// site, this helper does it automatically.
    ///
    /// # Parameters
    /// - `heap`: The heap to allocate from
    /// - `size`: Number of fields in the block
    /// - `tag`: Type of block (tuple, closure, etc.)
    ///
    /// # Returns
    /// Pointer to newly allocated block (fields uninitialized)
    fn alloc_block(&mut self, heap: &mut Heap, size: usize, tag: u8) -> Result<*mut Block> {
        let mut roots = Vec::new();
        self.collect_roots(&mut roots);
        heap.alloc_block(size, tag, &mut roots)
    }
    
    pub fn execute(&mut self, bytecode: &LoadedBytecode, heap: &mut Heap) -> Result<Value> {
        self.global_data = bytecode.data.clone();
        self.primitives.load_from_bytecode(&bytecode.primitives);
        
        self.pc = 0;
        self.run(&bytecode.code, heap)
    }
    
    fn run(&mut self, code: &[u32], heap: &mut Heap) -> Result<Value> {
        loop {
            if self.pc >= code.len() {
                return Ok(self.accu);
            }
            
            let instr = code[self.pc];
            self.pc += 1;
            
            match instr {
                Opcode::ACCESS_STACK0 => {
                    self.accu = self.stack[self.stack.len() - 1];
                }
                
                Opcode::ACCESS_STACK1 => {
                    self.accu = self.stack[self.stack.len() - 2];
                }
                
                Opcode::ACCESS_STACK2 => {
                    self.accu = self.stack[self.stack.len() - 3];
                }
                
                Opcode::ACCESS_STACK3 => {
                    self.accu = self.stack[self.stack.len() - 4];
                }
                
                Opcode::ACCESS_STACK4 => {
                    self.accu = self.stack[self.stack.len() - 5];
                }
                
                Opcode::ACCESS_STACK5 => {
                    self.accu = self.stack[self.stack.len() - 6];
                }
                
                Opcode::ACCESS_STACK6 => {
                    self.accu = self.stack[self.stack.len() - 7];
                }
                
                Opcode::ACCESS_STACK7 => {
                    self.accu = self.stack[self.stack.len() - 8];
                }
                
                Opcode::ACCESS_STACK => {
                    let n = code[self.pc] as usize;
                    self.pc += 1;
                    self.accu = self.stack[self.stack.len() - 1 - n];
                }
                
                Opcode::PUSH => {
                    self.stack.push(self.accu);
                }
                
                Opcode::PUSH_ACCESS_STACK0 => {
                    self.stack.push(self.accu);
                    self.accu = self.stack[self.stack.len() - 1];
                }
                
                Opcode::PUSH_ACCESS_STACK1 => {
                    self.stack.push(self.accu);
                    self.accu = self.stack[self.stack.len() - 2];
                }
                
                Opcode::PUSH_ACCESS_STACK2 => {
                    self.stack.push(self.accu);
                    self.accu = self.stack[self.stack.len() - 3];
                }
                
                Opcode::PUSH_ACCESS_STACK3 => {
                    self.stack.push(self.accu);
                    self.accu = self.stack[self.stack.len() - 4];
                }
                
                Opcode::PUSH_ACCESS_STACK4 => {
                    self.stack.push(self.accu);
                    self.accu = self.stack[self.stack.len() - 5];
                }
                
                Opcode::PUSH_ACCESS_STACK5 => {
                    self.stack.push(self.accu);
                    self.accu = self.stack[self.stack.len() - 6];
                }
                
                Opcode::PUSH_ACCESS_STACK6 => {
                    self.stack.push(self.accu);
                    self.accu = self.stack[self.stack.len() - 7];
                }
                
                Opcode::PUSH_ACCESS_STACK7 => {
                    self.stack.push(self.accu);
                    self.accu = self.stack[self.stack.len() - 8];
                }
                
                Opcode::PUSH_ACCESS_STACK => {
                    let n = code[self.pc] as usize;
                    self.pc += 1;
                    self.stack.push(self.accu);
                    self.accu = self.stack[self.stack.len() - 1 - n];
                }
                
                Opcode::ASSIGN_STACK => {
                    let n = code[self.pc] as usize;
                    self.pc += 1;
                    let len = self.stack.len();
                    self.stack[len - 1 - n] = self.accu;
                    self.accu = VAL_UNIT;
                }
                
                Opcode::POP => {
                    let n = code[self.pc] as usize;
                    self.pc += 1;
                    
                    if n > self.stack.len() {
                        return Err(Error::RuntimeError("Stack underflow".to_string()));
                    }
                    
                    self.stack.truncate(self.stack.len() - n);
                }
                
                Opcode::ACCESS_ENVIRONMENT1 => {
                    let env_block = self.env.as_block()
                        .ok_or_else(|| Error::RuntimeError("ENVACC1: env is not a block".to_string()))?;
                    self.accu = unsafe { (*env_block).field(1) };
                }
                
                Opcode::ACCESS_ENVIRONMENT2 => {
                    let env_block = self.env.as_block()
                        .ok_or_else(|| Error::RuntimeError("ENVACC2: env is not a block".to_string()))?;
                    self.accu = unsafe { (*env_block).field(2) };
                }
                
                Opcode::ACCESS_ENVIRONMENT3 => {
                    let env_block = self.env.as_block()
                        .ok_or_else(|| Error::RuntimeError("ENVACC3: env is not a block".to_string()))?;
                    self.accu = unsafe { (*env_block).field(3) };
                }
                
                Opcode::ACCESS_ENVIRONMENT4 => {
                    let env_block = self.env.as_block()
                        .ok_or_else(|| Error::RuntimeError("ENVACC4: env is not a block".to_string()))?;
                    self.accu = unsafe { (*env_block).field(4) };
                }
                
                Opcode::ACCESS_ENVIRONMENT => {
                    let n = code[self.pc] as usize;
                    self.pc += 1;
                    let env_block = self.env.as_block()
                        .ok_or_else(|| Error::RuntimeError("ENVACC: env is not a block".to_string()))?;
                    self.accu = unsafe { (*env_block).field(n) };
                }
                
                Opcode::PUSH_ACCESS_ENVIRONMENT1 => {
                    self.stack.push(self.accu);
                    let env_block = self.env.as_block()
                        .ok_or_else(|| Error::RuntimeError("PUSHENVACC1: env is not a block".to_string()))?;
                    self.accu = unsafe { (*env_block).field(1) };
                }
                
                Opcode::PUSH_ACCESS_ENVIRONMENT2 => {
                    self.stack.push(self.accu);
                    let env_block = self.env.as_block()
                        .ok_or_else(|| Error::RuntimeError("PUSHENVACC2: env is not a block".to_string()))?;
                    self.accu = unsafe { (*env_block).field(2) };
                }
                
                Opcode::PUSH_ACCESS_ENVIRONMENT3 => {
                    self.stack.push(self.accu);
                    let env_block = self.env.as_block()
                        .ok_or_else(|| Error::RuntimeError("PUSHENVACC3: env is not a block".to_string()))?;
                    self.accu = unsafe { (*env_block).field(3) };
                }
                
                Opcode::PUSH_ACCESS_ENVIRONMENT4 => {
                    self.stack.push(self.accu);
                    let env_block = self.env.as_block()
                        .ok_or_else(|| Error::RuntimeError("PUSHENVACC4: env is not a block".to_string()))?;
                    self.accu = unsafe { (*env_block).field(4) };
                }
                
                Opcode::PUSH_ACCESS_ENVIRONMENT => {
                    let n = code[self.pc] as usize;
                    self.pc += 1;
                    self.stack.push(self.accu);
                    let env_block = self.env.as_block()
                        .ok_or_else(|| Error::RuntimeError("PUSHENVACC: env is not a block".to_string()))?;
                    self.accu = unsafe { (*env_block).field(n) };
                }
                
                Opcode::GET_GLOBAL => {
                    let n = code[self.pc] as usize;
                    self.pc += 1;
                    if n >= self.global_data.len() {
                        return Err(Error::RuntimeError(format!("Global index {} out of bounds", n)));
                    }
                    self.accu = self.global_data[n];
                }
                
                Opcode::PUSH_GET_GLOBAL => {
                    let n = code[self.pc] as usize;
                    self.pc += 1;
                    self.stack.push(self.accu);
                    if n >= self.global_data.len() {
                        return Err(Error::RuntimeError(format!("Global index {} out of bounds", n)));
                    }
                    self.accu = self.global_data[n];
                }
                
                Opcode::GET_GLOBAL_FIELD => {
                    let n = code[self.pc] as usize;
                    self.pc += 1;
                    let p = code[self.pc] as usize;
                    self.pc += 1;
                    
                    if n >= self.global_data.len() {
                        return Err(Error::RuntimeError(format!("Global index {} out of bounds", n)));
                    }
                    
                    let global = self.global_data[n];
                    let block = global.as_block()
                        .ok_or_else(|| Error::RuntimeError("GETGLOBALFIELD: global is not a block".to_string()))?;
                    self.accu = unsafe { (*block).field(p) };
                }
                
                Opcode::SET_GLOBAL => {
                    let n = code[self.pc] as usize;
                    self.pc += 1;
                    if n >= self.global_data.len() {
                        self.global_data.resize(n + 1, VAL_UNIT);
                    }
                    self.global_data[n] = self.accu;
                    self.accu = VAL_UNIT;
                }
                
                Opcode::ATOM0 => {
                    let block = self.alloc_block(heap, 0, 0)?;
                    self.accu = Value::from_block_ptr(block);
                }
                
                Opcode::ATOM=> {
                    let tag = code[self.pc] as u8;
                    self.pc += 1;
                    let block = self.alloc_block(heap, 0, tag)?;
                    self.accu = Value::from_block_ptr(block);
                }
                
                Opcode::PUSH_ATOM => {
                    self.stack.push(self.accu);
                    let block = self.alloc_block(heap, 0, 0)?;
                    self.accu = Value::from_block_ptr(block);
                }
                
                Opcode::PUSH_ATOM=> {
                    let tag = code[self.pc] as u8;
                    self.pc += 1;
                    self.stack.push(self.accu);
                    let block = self.alloc_block(heap, 0, tag)?;
                    self.accu = Value::from_block_ptr(block);
                }
                
                Opcode::CONSTANT0 => {
                    self.accu = Value::int(0);
                }
                
                Opcode::CONSTANT1 => {
                    self.accu = Value::int(1);
                }
                
                Opcode::CONSTANT2 => {
                    self.accu = Value::int(2);
                }
                
                Opcode::CONSTANT3 => {
                    self.accu = Value::int(3);
                }
                
                Opcode::PUSH_CONSTANT0 => {
                    self.stack.push(self.accu);
                    self.accu = Value::int(0);
                }
                
                Opcode::PUSH_CONSTANT1 => {
                    self.stack.push(self.accu);
                    self.accu = Value::int(1);
                }
                
                Opcode::PUSH_CONSTANT2 => {
                    self.stack.push(self.accu);
                    self.accu = Value::int(2);
                }
                
                Opcode::PUSH_CONSTANT3 => {
                    self.stack.push(self.accu);
                    self.accu = Value::int(3);
                }
                
                Opcode::CONSTANT_INT => {
                    let n = code[self.pc] as i32;
                    self.pc += 1;
                    self.accu = Value::int(n as isize);
                }
                
                Opcode::PUSH_CONSTANT_INT => {
                    let n = code[self.pc] as i32;
                    self.pc += 1;
                    self.stack.push(self.accu);
                    self.accu = Value::int(n as isize);
                }
                
                Opcode::NEGATE_INTEGER => {
                    if !self.accu.is_int() {
                        return Err(Error::RuntimeError("NEGINT on non-integer".to_string()));
                    }
                    let n = self.accu.as_int();
                    self.accu = Value::int(-n);
                }
                
                Opcode::ADD_INTEGER => {
                    if !self.accu.is_int() {
                        return Err(Error::RuntimeError("ADDINT on non-integer".to_string()));
                    }
                    
                    let b = self.accu.as_int();
                    let a = self.stack.pop()
                        .ok_or_else(|| Error::RuntimeError("Stack underflow in ADDINT".to_string()))?;
                    
                    if !a.is_int() {
                        return Err(Error::RuntimeError("ADDINT on non-integer".to_string()));
                    }
                    
                    self.accu = Value::int(a.as_int() + b);
                }
                
                Opcode::SUBTRACT_INTEGER => {
                    if !self.accu.is_int() {
                        return Err(Error::RuntimeError("SUBINT on non-integer".to_string()));
                    }
                    
                    let b = self.accu.as_int();
                    let a = self.stack.pop()
                        .ok_or_else(|| Error::RuntimeError("Stack underflow in SUBINT".to_string()))?;
                    
                    if !a.is_int() {
                        return Err(Error::RuntimeError("SUBINT on non-integer".to_string()));
                    }
                    
                    self.accu = Value::int(a.as_int() - b);
                }
                
                Opcode::MULTIPLY_INTEGER => {
                    if !self.accu.is_int() {
                        return Err(Error::RuntimeError("MULINT on non-integer".to_string()));
                    }
                    
                    let b = self.accu.as_int();
                    let a = self.stack.pop()
                        .ok_or_else(|| Error::RuntimeError("Stack underflow in MULINT".to_string()))?;
                    
                    if !a.is_int() {
                        return Err(Error::RuntimeError("MULINT on non-integer".to_string()));
                    }
                    
                    self.accu = Value::int(a.as_int() * b);
                }
                
                Opcode::DIVIDE_INTEGER => {
                    if !self.accu.is_int() {
                        return Err(Error::RuntimeError("DIVINT on non-integer".to_string()));
                    }
                    
                    let b = self.accu.as_int();
                    if b == 0 {
                        return Err(Error::RuntimeError("Division by zero".to_string()));
                    }
                    
                    let a = self.stack.pop()
                        .ok_or_else(|| Error::RuntimeError("Stack underflow in DIVINT".to_string()))?;
                    
                    if !a.is_int() {
                        return Err(Error::RuntimeError("DIVINT on non-integer".to_string()));
                    }
                    
                    self.accu = Value::int(a.as_int() / b);
                }
                
                Opcode::MODULO_INTEGER => {
                    let b = self.accu.as_int();
                    if b == 0 {
                        return Err(Error::RuntimeError("Modulo by zero".to_string()));
                    }
                    let a = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?.as_int();
                    self.accu = Value::int(a % b);
                }
                
                Opcode::AND_INTEGER => {
                    let b = self.accu.as_int();
                    let a = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?.as_int();
                    self.accu = Value::int(a & b);
                }
                
                Opcode::OR_INTEGER => {
                    let b = self.accu.as_int();
                    let a = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?.as_int();
                    self.accu = Value::int(a | b);
                }
                
                Opcode::XOR_INTEGER => {
                    let b = self.accu.as_int();
                    let a = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?.as_int();
                    self.accu = Value::int(a ^ b);
                }
                
                Opcode::LOGICAL_SHIFT_LEFT_INTEGER => {
                    let b = self.accu.as_int();
                    let a = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?.as_int();
                    self.accu = Value::int(a << b);
                }
                
                Opcode::LOGICAL_SHIFT_RIGHT_INTEGER => {
                    let b = self.accu.as_int();
                    let a = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?.as_int();
                    self.accu = Value::int((a as usize >> b) as isize);
                }
                
                Opcode::ARITHMETIC_SHIFT_RIGHT_INTEGER => {
                    let b = self.accu.as_int();
                    let a = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?.as_int();
                    self.accu = Value::int(a >> b);
                }
                
                Opcode::EQUAL => {
                    let b = self.accu;
                    let a = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?;
                    self.accu = Value::int(if a == b { 1 } else { 0 });
                }
                
                Opcode::NOT_EQUAL => {
                    let b = self.accu;
                    let a = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?;
                    self.accu = Value::int(if a != b { 1 } else { 0 });
                }
                
                Opcode::LESS_THAN_INTEGER => {
                    let b = self.accu.as_int();
                    let a = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?.as_int();
                    self.accu = Value::int(if a < b { 1 } else { 0 });
                }
                
                Opcode::LESS_EQUAL_INTEGER => {
                    let b = self.accu.as_int();
                    let a = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?.as_int();
                    self.accu = Value::int(if a <= b { 1 } else { 0 });
                }
                
                Opcode::GREATER_THAN_INTEGER => {
                    let b = self.accu.as_int();
                    let a = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?.as_int();
                    self.accu = Value::int(if a > b { 1 } else { 0 });
                }
                
                Opcode::GREATER_EQUAL_INTEGER => {
                    let b = self.accu.as_int();
                    let a = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?.as_int();
                    self.accu = Value::int(if a >= b { 1 } else { 0 });
                }
                
                Opcode::OFFSET_INTEGER => {
                    let offset = code[self.pc] as i32;
                    self.pc += 1;
                    self.accu = Value::int(self.accu.as_int() + offset as isize);
                }
                
                Opcode::OFFSET_REF => {
                    let offset = code[self.pc] as i32;
                    self.pc += 1;
                    let block = self.accu.as_block_mut()
                        .ok_or_else(|| Error::RuntimeError("OFFSETREF on non-block".to_string()))?;
                    unsafe {
                        let val = (*block).field(0);
                        (*block).set_field(0, Value::int(val.as_int() + offset as isize));
                    }
                    self.accu = VAL_UNIT;
                }
                
                Opcode::IS_INTEGER => {
                    self.accu = Value::int(if self.accu.is_int() { 1 } else { 0 });
                }
                
                Opcode::BOOLEAN_NOT => {
                    self.accu = Value::int(if self.accu.as_int() == 0 { 1 } else { 0 });
                }
                
                Opcode::UNSIGNED_LESS_THAN_INTEGER => {
                    let b = self.accu.as_int() as usize;
                    let a = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?.as_int() as usize;
                    self.accu = Value::int(if a < b { 1 } else { 0 });
                }
                
                Opcode::UNSIGNED_GREATER_EQUAL_INTEGER => {
                    let b = self.accu.as_int() as usize;
                    let a = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?.as_int() as usize;
                    self.accu = Value::int(if a >= b { 1 } else { 0 });
                }
                
                Opcode::BRANCH => {
                    let offset = code[self.pc] as i32;
                    self.pc = ((self.pc as isize) + (offset as isize)) as usize;
                }
                
                Opcode::BRANCH_IF => {
                    let offset = code[self.pc] as i32;
                    if self.accu.as_int() != 0 {
                        self.pc = ((self.pc as isize) + (offset as isize)) as usize;
                    } else {
                        self.pc += 1;
                    }
                }
                
                Opcode::BRANCH_IF_NOT => {
                    let offset = code[self.pc] as i32;
                    if self.accu.as_int() == 0 {
                        self.pc = ((self.pc as isize) + (offset as isize)) as usize;
                    } else {
                        self.pc += 1;
                    }
                }
                
                Opcode::SWITCH => {
                    let sizes = code[self.pc];
                    self.pc += 1;
                    
                    let n_int_cases = (sizes & 0xFFFF) as usize;
                    let n_tag_cases = (sizes >> 16) as usize;
                    
                    let index = if self.accu.is_int() {
                        self.accu.as_int() as usize
                    } else {
                        let block = self.accu.as_block()
                            .ok_or_else(|| Error::RuntimeError("SWITCH on invalid value".to_string()))?;
                        n_int_cases + unsafe { (*block).tag() as usize }
                    };
                    
                    let offset_pos = self.pc + index;
                    if offset_pos >= code.len() {
                        return Err(Error::RuntimeError(format!("SWITCH: offset out of bounds")));
                    }
                    
                    let offset = code[offset_pos] as i32;
                    self.pc = ((offset_pos as isize) + (offset as isize)) as usize;
                }
                
                Opcode::BRANCH_EQUAL => {
                    let n = code[self.pc] as i32;
                    self.pc += 1;
                    let offset = code[self.pc] as i32;
                    if self.accu.as_int() == n as isize {
                        self.pc = ((self.pc as isize) + (offset as isize)) as usize;
                    } else {
                        self.pc += 1;
                    }
                }
                
                Opcode::BRANCH_NOT_EQUAL => {
                    let n = code[self.pc] as i32;
                    self.pc += 1;
                    let offset = code[self.pc] as i32;
                    if self.accu.as_int() != n as isize {
                        self.pc = ((self.pc as isize) + (offset as isize)) as usize;
                    } else {
                        self.pc += 1;
                    }
                }
                
                Opcode::BRANCH_LESS_THAN_INTEGER => {
                    let n = code[self.pc] as i32;
                    self.pc += 1;
                    let offset = code[self.pc] as i32;
                    if self.accu.as_int() < n as isize {
                        self.pc = ((self.pc as isize) + (offset as isize)) as usize;
                    } else {
                        self.pc += 1;
                    }
                }
                
                Opcode::BRANCH_LESS_EQUAL_INTEGER => {
                    let n = code[self.pc] as i32;
                    self.pc += 1;
                    let offset = code[self.pc] as i32;
                    if self.accu.as_int() <= n as isize {
                        self.pc = ((self.pc as isize) + (offset as isize)) as usize;
                    } else {
                        self.pc += 1;
                    }
                }
                
                Opcode::BRANCH_GREATER_THAN_INTEGER => {
                    let n = code[self.pc] as i32;
                    self.pc += 1;
                    let offset = code[self.pc] as i32;
                    if self.accu.as_int() > n as isize {
                        self.pc = ((self.pc as isize) + (offset as isize)) as usize;
                    } else {
                        self.pc += 1;
                    }
                }
                
                Opcode::BRANCH_GREATER_EQUAL_INTEGER => {
                    let n = code[self.pc] as i32;
                    self.pc += 1;
                    let offset = code[self.pc] as i32;
                    if self.accu.as_int() >= n as isize {
                        self.pc = ((self.pc as isize) + (offset as isize)) as usize;
                    } else {
                        self.pc += 1;
                    }
                }
                
                Opcode::BRANCH_UNSIGNED_LESS_THAN_INTEGER => {
                    let n = code[self.pc] as i32;
                    self.pc += 1;
                    let offset = code[self.pc] as i32;
                    if (self.accu.as_int() as usize) < (n as usize) {
                        self.pc = ((self.pc as isize) + (offset as isize)) as usize;
                    } else {
                        self.pc += 1;
                    }
                }
                
                Opcode::BRANCH_UNSIGNED_GREATER_EQUAL_INTEGER => {
                    let n = code[self.pc] as i32;
                    self.pc += 1;
                    let offset = code[self.pc] as i32;
                    if (self.accu.as_int() as usize) >= (n as usize) {
                        self.pc = ((self.pc as isize) + (offset as isize)) as usize;
                    } else {
                        self.pc += 1;
                    }
                }
                
                Opcode::MAKE_BLOCK=> {
                    let size = code[self.pc] as usize;
                    self.pc += 1;
                    let tag = code[self.pc] as u8;
                    self.pc += 1;
                    
                    let block = self.alloc_block(heap, size, tag)?;
                    
                    for i in 0..size {
                        let val = self.stack.pop()
                            .ok_or_else(|| Error::RuntimeError("Stack underflow in MAKEBLOCK".to_string()))?;
                        unsafe {
                            (*block).set_field(size - 1 - i, val);
                        }
                    }
                    
                    self.accu = Value::from_block_ptr(block);
                }
                
                Opcode::MAKE_BLOCK1 => {
                    let tag = code[self.pc] as u8;
                    self.pc += 1;
                    
                    let block = self.alloc_block(heap, 1, tag)?;
                    unsafe {
                        (*block).set_field(0, self.accu);
                    }
                    
                    self.accu = Value::from_block_ptr(block);
                }
                
                Opcode::MAKE_BLOCK2 => {
                    let tag = code[self.pc] as u8;
                    self.pc += 1;
                    
                    let block = self.alloc_block(heap, 2, tag)?;
                    let v0 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?;
                    unsafe {
                        (*block).set_field(0, v0);
                        (*block).set_field(1, self.accu);
                    }
                    
                    self.accu = Value::from_block_ptr(block);
                }
                
                Opcode::MAKE_BLOCK3 => {
                    let tag = code[self.pc] as u8;
                    self.pc += 1;
                    
                    let block = self.alloc_block(heap, 3, tag)?;
                    let v1 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?;
                    let v0 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?;
                    unsafe {
                        (*block).set_field(0, v0);
                        (*block).set_field(1, v1);
                        (*block).set_field(2, self.accu);
                    }
                    
                    self.accu = Value::from_block_ptr(block);
                }
                
                Opcode::GET_FIELD0 => {
                    let block = self.accu.as_block()
                        .ok_or_else(|| Error::RuntimeError("GETFIELD0 on non-block".to_string()))?;
                    self.accu = unsafe { (*block).field(0) };
                }
                
                Opcode::GET_FIELD1 => {
                    let block = self.accu.as_block()
                        .ok_or_else(|| Error::RuntimeError("GETFIELD1 on non-block".to_string()))?;
                    self.accu = unsafe { (*block).field(1) };
                }
                
                Opcode::GET_FIELD2 => {
                    let block = self.accu.as_block()
                        .ok_or_else(|| Error::RuntimeError("GETFIELD2 on non-block".to_string()))?;
                    self.accu = unsafe { (*block).field(2) };
                }
                
                Opcode::GET_FIELD3 => {
                    let block = self.accu.as_block()
                        .ok_or_else(|| Error::RuntimeError("GETFIELD3 on non-block".to_string()))?;
                    self.accu = unsafe { (*block).field(3) };
                }
                
                Opcode::GET_FIELD=> {
                    let n = code[self.pc] as usize;
                    self.pc += 1;
                    
                    let block = self.accu.as_block()
                        .ok_or_else(|| Error::RuntimeError("GETFIELD on non-block".to_string()))?;
                    self.accu = unsafe { (*block).field(n) };
                }
                
                Opcode::SET_FIELD0 => {
                    let block = self.accu.as_block_mut()
                        .ok_or_else(|| Error::RuntimeError("SETFIELD0 on non-block".to_string()))?;
                    
                    let val = self.stack.pop()
                        .ok_or_else(|| Error::RuntimeError("Stack underflow in SETFIELD0".to_string()))?;
                    
                    unsafe {
                        (*block).set_field(0, val);
                    }
                    
                    // Write barrier: track old→young pointers for GC
                    heap.write_barrier(block, val);
                    
                    self.accu = VAL_UNIT;
                }
                
                Opcode::SET_FIELD1 => {
                    let block = self.accu.as_block_mut()
                        .ok_or_else(|| Error::RuntimeError("SETFIELD1 on non-block".to_string()))?;
                    let val = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?;
                    unsafe { (*block).set_field(1, val); }
                    heap.write_barrier(block, val);
                    self.accu = VAL_UNIT;
                }
                
                Opcode::SET_FIELD2 => {
                    let block = self.accu.as_block_mut()
                        .ok_or_else(|| Error::RuntimeError("SETFIELD2 on non-block".to_string()))?;
                    let val = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?;
                    unsafe { (*block).set_field(2, val); }
                    heap.write_barrier(block, val);
                    self.accu = VAL_UNIT;
                }
                
                Opcode::SET_FIELD3 => {
                    let block = self.accu.as_block_mut()
                        .ok_or_else(|| Error::RuntimeError("SETFIELD3 on non-block".to_string()))?;
                    let val = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?;
                    unsafe { (*block).set_field(3, val); }
                    heap.write_barrier(block, val);
                    self.accu = VAL_UNIT;
                }
                
                Opcode::SET_FIELD=> {
                    let n = code[self.pc] as usize;
                    self.pc += 1;
                    let block = self.accu.as_block_mut()
                        .ok_or_else(|| Error::RuntimeError("SETFIELD on non-block".to_string()))?;
                    let val = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?;
                    unsafe { (*block).set_field(n, val); }
                    heap.write_barrier(block, val);
                    self.accu = VAL_UNIT;
                }
                
                Opcode::VECTOR_LENGTH => {
                    let block = self.accu.as_block()
                        .ok_or_else(|| Error::RuntimeError("VECTLENGTH on non-block".to_string()))?;
                    let len = unsafe { (*block).size() };
                    self.accu = Value::int(len as isize);
                }
                
                Opcode::GET_VECTOR_ITEM => {
                    let index = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?;
                    let block = self.accu.as_block()
                        .ok_or_else(|| Error::RuntimeError("GETVECTITEM on non-block".to_string()))?;
                    let idx = index.as_int() as usize;
                    self.accu = unsafe { (*block).field(idx) };
                }
                
                Opcode::SET_VECTOR_ITEM => {
                    let val = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?;
                    let index = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow".to_string()))?;
                    let block = self.accu.as_block_mut()
                        .ok_or_else(|| Error::RuntimeError("SETVECTITEM on non-block".to_string()))?;
                    let idx = index.as_int() as usize;
                    unsafe { (*block).set_field(idx, val); }
                    heap.write_barrier(block, val);
                    self.accu = VAL_UNIT;
                }
                
                Opcode::CLOSURE => {
                    let nvars = code[self.pc] as usize;
                    self.pc += 1;
                    let offset = code[self.pc] as i32;
                    self.pc += 1;
                    
                    let code_ptr = ((self.pc as isize) + (offset as isize) - 1) as usize;
                    
                    let closure = self.alloc_block(heap, nvars + 2, crate::value::Tag::CLOSURE)?;
                    unsafe {
                        (*closure).set_field(0, Value::int(code_ptr as isize));
                        (*closure).set_field(1, Value::int(nvars as isize));
                        for i in 0..nvars {
                            let val = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in CLOSURE".to_string()))?;
                            (*closure).set_field(2 + nvars - 1 - i, val);
                        }
                    }
                    
                    self.accu = Value::from_block_ptr(closure);
                }
                
                Opcode::CLOSURE_REC => {
                    let nfuncs = code[self.pc] as usize;
                    self.pc += 1;
                    let nvars = code[self.pc] as usize;
                    self.pc += 1;
                    
                    let mut offsets = Vec::with_capacity(nfuncs);
                    for _ in 0..nfuncs {
                        let offset = code[self.pc] as i32;
                        self.pc += 1;
                        offsets.push(offset);
                    }
                    
                    let mut closures = Vec::with_capacity(nfuncs);
                    for &offset in &offsets {
                        let code_ptr = ((self.pc as isize) + (offset as isize) - 1) as usize;
                        let closure = self.alloc_block(heap, nvars + nfuncs + 2, crate::value::Tag::CLOSURE)?;
                        unsafe {
                            (*closure).set_field(0, Value::int(code_ptr as isize));
                            (*closure).set_field(1, Value::int((nvars + nfuncs) as isize));
                        }
                        closures.push(closure);
                    }
                    
                    for i in 0..nfuncs {
                        let closure = closures[i];
                        unsafe {
                            for j in 0..nfuncs {
                                (*closure).set_field(2 + j, Value::from_block_ptr(closures[j]));
                            }
                            
                            for k in 0..nvars {
                                let val = self.stack[self.stack.len() - nvars + k];
                                (*closure).set_field(2 + nfuncs + k, val);
                            }
                        }
                    }
                    
                    for _ in 0..nvars {
                        self.stack.pop();
                    }
                    
                    self.accu = Value::from_block_ptr(closures[0]);
                    
                    for i in 1..nfuncs {
                        self.stack.push(Value::from_block_ptr(closures[i]));
                    }
                }
                
                Opcode::APPLY => {
                    let nargs = code[self.pc] as isize;
                    self.pc += 1;
                    
                    let closure = self.accu.as_block()
                        .ok_or_else(|| Error::RuntimeError("APPLY on non-closure".to_string()))?;
                    
                    if unsafe { (*closure).tag() } != crate::value::Tag::CLOSURE {
                        return Err(Error::RuntimeError("APPLY on non-closure tag".to_string()));
                    }
                    
                    let code_ptr = unsafe { (*closure).field(0).as_int() as usize };
                    
                    self.stack.push(Value::from_raw(self.env.as_raw()));
                    self.stack.push(Value::int(self.extra_args));
                    self.stack.push(Value::int(self.pc as isize));
                    
                    self.env = self.accu;
                    self.extra_args = nargs - 1;
                    self.pc = code_ptr;
                }
                
                Opcode::APPLY1 => {
                    let closure = self.accu.as_block()
                        .ok_or_else(|| Error::RuntimeError("APPLY1 on non-closure".to_string()))?;
                    
                    if unsafe { (*closure).tag() } != crate::value::Tag::CLOSURE {
                        return Err(Error::RuntimeError("APPLY1 on non-closure tag".to_string()));
                    }
                    
                    let code_ptr = unsafe { (*closure).field(0).as_int() as usize };
                    
                    self.stack.push(Value::from_raw(self.env.as_raw()));
                    self.stack.push(Value::int(self.extra_args));
                    self.stack.push(Value::int(self.pc as isize));
                    
                    self.env = self.accu;
                    self.extra_args = 0;
                    self.pc = code_ptr;
                }
                
                Opcode::APPLY2 => {
                    let closure = self.accu.as_block()
                        .ok_or_else(|| Error::RuntimeError("APPLY2 on non-closure".to_string()))?;
                    
                    if unsafe { (*closure).tag() } != crate::value::Tag::CLOSURE {
                        return Err(Error::RuntimeError("APPLY2 on non-closure tag".to_string()));
                    }
                    
                    let code_ptr = unsafe { (*closure).field(0).as_int() as usize };
                    
                    self.stack.push(Value::from_raw(self.env.as_raw()));
                    self.stack.push(Value::int(self.extra_args));
                    self.stack.push(Value::int(self.pc as isize));
                    
                    self.env = self.accu;
                    self.extra_args = 1;
                    self.pc = code_ptr;
                }
                
                Opcode::APPLY3 => {
                    let closure = self.accu.as_block()
                        .ok_or_else(|| Error::RuntimeError("APPLY3 on non-closure".to_string()))?;
                    
                    if unsafe { (*closure).tag() } != crate::value::Tag::CLOSURE {
                        return Err(Error::RuntimeError("APPLY3 on non-closure tag".to_string()));
                    }
                    
                    let code_ptr = unsafe { (*closure).field(0).as_int() as usize };
                    
                    self.stack.push(Value::from_raw(self.env.as_raw()));
                    self.stack.push(Value::int(self.extra_args));
                    self.stack.push(Value::int(self.pc as isize));
                    
                    self.env = self.accu;
                    self.extra_args = 2;
                    self.pc = code_ptr;
                }
                
                Opcode::RETURN => {
                    let n = code[self.pc] as usize;
                    self.pc += 1;
                    
                    for _ in 0..n {
                        self.stack.pop();
                    }
                    
                    if self.extra_args > 0 {
                        self.extra_args -= 1;
                        
                        let closure = self.accu.as_block()
                            .ok_or_else(|| Error::RuntimeError("RETURN: accu not a closure for extra args".to_string()))?;
                        
                        if unsafe { (*closure).tag() } != crate::value::Tag::CLOSURE {
                            return Err(Error::RuntimeError("RETURN: accu not a closure tag".to_string()));
                        }
                        
                        let code_ptr = unsafe { (*closure).field(0).as_int() as usize };
                        self.env = self.accu;
                        self.pc = code_ptr;
                    } else {
                        let return_addr = self.stack.pop()
                            .ok_or_else(|| Error::RuntimeError("Stack underflow in RETURN".to_string()))?
                            .as_int() as usize;
                        let saved_extra_args = self.stack.pop()
                            .ok_or_else(|| Error::RuntimeError("Stack underflow in RETURN".to_string()))?
                            .as_int();
                        let saved_env = self.stack.pop()
                            .ok_or_else(|| Error::RuntimeError("Stack underflow in RETURN".to_string()))?;
                        
                        self.pc = return_addr;
                        self.extra_args = saved_extra_args;
                        self.env = saved_env;
                    }
                }
                
                Opcode::GRAB => {
                    let required = code[self.pc] as isize;
                    self.pc += 1;
                    
                    if self.extra_args >= required {
                        self.extra_args -= required;
                    } else {
                        let num_args = self.extra_args + 1;
                        
                        let closure = self.alloc_block(heap, num_args as usize + 2, crate::value::Tag::CLOSURE)?;
                        unsafe {
                            (*closure).set_field(0, Value::int((self.pc - 1) as isize));
                            (*closure).set_field(1, Value::int(num_args));
                            
                            for i in 0..num_args {
                                let val = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in GRAB".to_string()))?;
                                (*closure).set_field(2 + num_args as usize - 1 - i as usize, val);
                            }
                        }
                        
                        self.accu = Value::from_block_ptr(closure);
                        
                        let return_addr = self.stack.pop()
                            .ok_or_else(|| Error::RuntimeError("Stack underflow in GRAB".to_string()))?
                            .as_int() as usize;
                        let saved_extra_args = self.stack.pop()
                            .ok_or_else(|| Error::RuntimeError("Stack underflow in GRAB".to_string()))?
                            .as_int();
                        let saved_env = self.stack.pop()
                            .ok_or_else(|| Error::RuntimeError("Stack underflow in GRAB".to_string()))?;
                        
                        self.pc = return_addr;
                        self.extra_args = saved_extra_args;
                        self.env = saved_env;
                    }
                }
                
                Opcode::RESTART => {
                    let closure = self.env.as_block()
                        .ok_or_else(|| Error::RuntimeError("RESTART: env is not a closure".to_string()))?;
                    
                    let num_args = unsafe { (*closure).field(1).as_int() };
                    
                    for i in 0..num_args {
                        let arg = unsafe { (*closure).field(2 + i as usize) };
                        self.stack.push(arg);
                    }
                    
                    self.env = unsafe { (*closure).field(2 + num_args as usize) };
                    self.extra_args = num_args - 1;
                }
                
                Opcode::APP_TERM => {
                    let nargs = code[self.pc] as isize;
                    self.pc += 1;
                    let slotsize = code[self.pc] as usize;
                    self.pc += 1;
                    
                    let closure = self.accu.as_block()
                        .ok_or_else(|| Error::RuntimeError("APPTERM on non-closure".to_string()))?;
                    
                    if unsafe { (*closure).tag() } != crate::value::Tag::CLOSURE {
                        return Err(Error::RuntimeError("APPTERM on non-closure tag".to_string()));
                    }
                    
                    let code_ptr = unsafe { (*closure).field(0).as_int() as usize };
                    
                    let mut args = Vec::with_capacity(nargs as usize);
                    for _ in 0..nargs {
                        args.push(self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in APPTERM".to_string()))?);
                    }
                    
                    for _ in 0..slotsize {
                        self.stack.pop();
                    }
                    
                    for arg in args.iter().rev() {
                        self.stack.push(*arg);
                    }
                    
                    self.env = self.accu;
                    self.extra_args = nargs - 1;
                    self.pc = code_ptr;
                }
                
                Opcode::APP_TERM1 => {
                    let slotsize = code[self.pc] as usize;
                    self.pc += 1;
                    
                    let closure = self.accu.as_block()
                        .ok_or_else(|| Error::RuntimeError("APPTERM1 on non-closure".to_string()))?;
                    
                    if unsafe { (*closure).tag() } != crate::value::Tag::CLOSURE {
                        return Err(Error::RuntimeError("APPTERM1 on non-closure tag".to_string()));
                    }
                    
                    let code_ptr = unsafe { (*closure).field(0).as_int() as usize };
                    let arg0 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in APPTERM1".to_string()))?;
                    
                    for _ in 0..slotsize {
                        self.stack.pop();
                    }
                    
                    self.stack.push(arg0);
                    self.env = self.accu;
                    self.extra_args = 0;
                    self.pc = code_ptr;
                }
                
                Opcode::APP_TERM2 => {
                    let slotsize = code[self.pc] as usize;
                    self.pc += 1;
                    
                    let closure = self.accu.as_block()
                        .ok_or_else(|| Error::RuntimeError("APPTERM2 on non-closure".to_string()))?;
                    
                    if unsafe { (*closure).tag() } != crate::value::Tag::CLOSURE {
                        return Err(Error::RuntimeError("APPTERM2 on non-closure tag".to_string()));
                    }
                    
                    let code_ptr = unsafe { (*closure).field(0).as_int() as usize };
                    let arg1 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in APPTERM2".to_string()))?;
                    let arg0 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in APPTERM2".to_string()))?;
                    
                    for _ in 0..slotsize {
                        self.stack.pop();
                    }
                    
                    self.stack.push(arg0);
                    self.stack.push(arg1);
                    self.env = self.accu;
                    self.extra_args = 1;
                    self.pc = code_ptr;
                }
                
                Opcode::APP_TERM3 => {
                    let slotsize = code[self.pc] as usize;
                    self.pc += 1;
                    
                    let closure = self.accu.as_block()
                        .ok_or_else(|| Error::RuntimeError("APPTERM3 on non-closure".to_string()))?;
                    
                    if unsafe { (*closure).tag() } != crate::value::Tag::CLOSURE {
                        return Err(Error::RuntimeError("APPTERM3 on non-closure tag".to_string()));
                    }
                    
                    let code_ptr = unsafe { (*closure).field(0).as_int() as usize };
                    let arg2 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in APPTERM3".to_string()))?;
                    let arg1 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in APPTERM3".to_string()))?;
                    let arg0 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in APPTERM3".to_string()))?;
                    
                    for _ in 0..slotsize {
                        self.stack.pop();
                    }
                    
                    self.stack.push(arg0);
                    self.stack.push(arg1);
                    self.stack.push(arg2);
                    self.env = self.accu;
                    self.extra_args = 2;
                    self.pc = code_ptr;
                }
                
                Opcode::OFFSET_CLOSURE0 => {
                    self.accu = self.env;
                }
                
                Opcode::OFFSET_CLOSURE_M2 => {
                    let env_block = self.env.as_block()
                        .ok_or_else(|| Error::RuntimeError("OFFSETCLOSUREM2: env is not a block".to_string()))?;
                    self.accu = unsafe { (*env_block).field(0) };
                }
                
                Opcode::OFFSET_CLOSURE2 => {
                    let env_block = self.env.as_block()
                        .ok_or_else(|| Error::RuntimeError("OFFSETCLOSURE2: env is not a block".to_string()))?;
                    self.accu = unsafe { (*env_block).field(2) };
                }
                
                Opcode::OFFSET_CLOSURE => {
                    let n = code[self.pc] as usize;
                    self.pc += 1;
                    let env_block = self.env.as_block()
                        .ok_or_else(|| Error::RuntimeError("OFFSETCLOSURE: env is not a block".to_string()))?;
                    self.accu = unsafe { (*env_block).field(n) };
                }
                
                Opcode::PUSH_RETURN_ADDRESS => {
                    let offset = code[self.pc] as i32;
                    self.pc += 1;
                    let return_addr = ((self.pc as isize) + (offset as isize)) as usize;
                    self.stack.push(Value::from_raw(self.env.as_raw()));
                    self.stack.push(Value::int(self.extra_args));
                    self.stack.push(Value::int(return_addr as isize));
                }
                
                Opcode::C_CALL_1 => {
                    let prim_index = code[self.pc] as usize;
                    self.pc += 1;
                    self.accu = self.call_primitive(prim_index, &[self.accu], heap)?;
                }
                
                Opcode::C_CALL_2 => {
                    let prim_index = code[self.pc] as usize;
                    self.pc += 1;
                    let arg1 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in C_CALL_2".to_string()))?;
                    self.accu = self.call_primitive(prim_index, &[self.accu, arg1], heap)?;
                }
                
                Opcode::C_CALL_3 => {
                    let prim_index = code[self.pc] as usize;
                    self.pc += 1;
                    let arg2 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in C_CALL_3".to_string()))?;
                    let arg1 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in C_CALL_3".to_string()))?;
                    self.accu = self.call_primitive(prim_index, &[self.accu, arg1, arg2], heap)?;
                }
                
                Opcode::C_CALL_4 => {
                    let prim_index = code[self.pc] as usize;
                    self.pc += 1;
                    let arg3 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in C_CALL_4".to_string()))?;
                    let arg2 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in C_CALL_4".to_string()))?;
                    let arg1 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in C_CALL_4".to_string()))?;
                    self.accu = self.call_primitive(prim_index, &[self.accu, arg1, arg2, arg3], heap)?;
                }
                
                Opcode::C_CALL_5 => {
                    let prim_index = code[self.pc] as usize;
                    self.pc += 1;
                    let arg4 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in C_CALL_5".to_string()))?;
                    let arg3 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in C_CALL_5".to_string()))?;
                    let arg2 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in C_CALL_5".to_string()))?;
                    let arg1 = self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in C_CALL_5".to_string()))?;
                    self.accu = self.call_primitive(prim_index, &[self.accu, arg1, arg2, arg3, arg4], heap)?;
                }
                
                Opcode::C_CALL_N => {
                    let nargs = code[self.pc] as usize;
                    self.pc += 1;
                    let prim_index = code[self.pc] as usize;
                    self.pc += 1;
                    
                    let mut args = Vec::with_capacity(nargs);
                    for _ in 0..nargs - 1 {
                        args.push(self.stack.pop().ok_or_else(|| Error::RuntimeError("Stack underflow in C_CALL_N".to_string()))?);
                    }
                    args.push(self.accu);
                    args.reverse();
                    
                    self.accu = self.call_primitive(prim_index, &args, heap)?;
                }
                
                Opcode::PUSH_TRAP => {
                    let offset = code[self.pc] as i32;
                    self.pc += 1;
                    
                    let handler_pc = ((self.pc as isize) + (offset as isize)) as usize;
                    
                    self.stack.push(Value::from_raw(self.env.as_raw()));
                    self.stack.push(Value::int(self.extra_args));
                    self.stack.push(Value::int(handler_pc as isize));
                    self.stack.push(Value::int(self.trap_sp.unwrap_or(0) as isize));
                    
                    self.trap_sp = Some(self.stack.len());
                }
                
                Opcode::POP_TRAP => {
                    if let Some(trap_sp) = self.trap_sp {
                        if self.stack.len() >= trap_sp {
                            self.stack.truncate(trap_sp - 4);
                            
                            if trap_sp >= 4 {
                                let prev_trap = self.stack.get(trap_sp - 4)
                                    .map(|v| v.as_int() as usize);
                                self.trap_sp = if prev_trap == Some(0) { None } else { prev_trap };
                            } else {
                                self.trap_sp = None;
                            }
                        }
                    }
                }
                
                Opcode::RAISE => {
                    if let Some(trap_sp) = self.trap_sp {
                        if trap_sp <= self.stack.len() && trap_sp >= 4 {
                            let prev_trap_sp = self.stack[trap_sp - 4].as_int() as usize;
                            let handler_pc = self.stack[trap_sp - 3].as_int() as usize;
                            let saved_extra_args = self.stack[trap_sp - 2].as_int();
                            let saved_env = self.stack[trap_sp - 1];
                            
                            self.stack.truncate(trap_sp - 4);
                            
                            self.trap_sp = if prev_trap_sp == 0 { None } else { Some(prev_trap_sp) };
                            self.pc = handler_pc;
                            self.extra_args = saved_extra_args;
                            self.env = saved_env;
                        } else {
                            return Err(Error::RuntimeError(format!("Uncaught exception: {:?}", self.accu)));
                        }
                    } else {
                        return Err(Error::RuntimeError(format!("Uncaught exception: {:?}", self.accu)));
                    }
                }
                
                Opcode::RERAISE => {
                    if let Some(trap_sp) = self.trap_sp {
                        if trap_sp > 4 && trap_sp <= self.stack.len() {
                            let prev_prev_trap_sp = if trap_sp >= 8 {
                                self.stack[trap_sp - 8].as_int() as usize
                            } else {
                                0
                            };
                            
                            self.trap_sp = if prev_prev_trap_sp == 0 { None } else { Some(prev_prev_trap_sp) };
                        }
                    }
                    
                    if let Some(trap_sp) = self.trap_sp {
                        if trap_sp <= self.stack.len() && trap_sp >= 4 {
                            let prev_trap_sp = self.stack[trap_sp - 4].as_int() as usize;
                            let handler_pc = self.stack[trap_sp - 3].as_int() as usize;
                            let saved_extra_args = self.stack[trap_sp - 2].as_int();
                            let saved_env = self.stack[trap_sp - 1];
                            
                            self.stack.truncate(trap_sp - 4);
                            
                            self.trap_sp = if prev_trap_sp == 0 { None } else { Some(prev_trap_sp) };
                            self.pc = handler_pc;
                            self.extra_args = saved_extra_args;
                            self.env = saved_env;
                        } else {
                            return Err(Error::RuntimeError(format!("Uncaught exception (reraise): {:?}", self.accu)));
                        }
                    } else {
                        return Err(Error::RuntimeError(format!("Uncaught exception (reraise): {:?}", self.accu)));
                    }
                }
                
                Opcode::RAISE_NOTRACE => {
                    if let Some(trap_sp) = self.trap_sp {
                        if trap_sp <= self.stack.len() && trap_sp >= 4 {
                            let prev_trap_sp = self.stack[trap_sp - 4].as_int() as usize;
                            let handler_pc = self.stack[trap_sp - 3].as_int() as usize;
                            let saved_extra_args = self.stack[trap_sp - 2].as_int();
                            let saved_env = self.stack[trap_sp - 1];
                            
                            self.stack.truncate(trap_sp - 4);
                            
                            self.trap_sp = if prev_trap_sp == 0 { None } else { Some(prev_trap_sp) };
                            self.pc = handler_pc;
                            self.extra_args = saved_extra_args;
                            self.env = saved_env;
                        } else {
                            return Err(Error::RuntimeError(format!("Uncaught exception: {:?}", self.accu)));
                        }
                    } else {
                        return Err(Error::RuntimeError(format!("Uncaught exception: {:?}", self.accu)));
                    }
                }
                
                Opcode::CHECK_SIGNALS => {
                }
                
                Opcode::PERFORM => {
                    let effect_val = self.accu;
                    
                    if self.current_handler.is_none() {
                        return Err(Error::RuntimeError(format!("Unhandled effect: {:?}", effect_val)));
                    }
                    
                    let cont = Continuation::capture(
                        self.stack.clone(),
                        self.pc,
                        VAL_UNIT,
                        self.env,
                        self.extra_args,
                        self.trap_sp,
                        self.current_handler.clone(),
                    );
                    
                    let cont_index = self.continuations.len();
                    self.continuations.push(cont);
                    
                    let cont_block = self.alloc_block(heap, 1, crate::value::Tag::CONTINUATION)?;
                    unsafe {
                        (*cont_block).set_field(0, Value::int(cont_index as isize));
                    }
                    let cont_value = Value::from_block_ptr(cont_block);
                    
                    if let Some(handler) = self.current_handler.take() {
                        if let Some(parent_stack) = handler.parent_stack {
                            self.stack = parent_stack;
                        } else {
                            self.stack.clear();
                        }
                        
                        if let Some(parent_pc) = handler.parent_pc {
                            self.pc = parent_pc;
                        }
                        
                        if let Some(parent_env) = handler.parent_env {
                            self.env = parent_env;
                        }
                        
                        if let Some(parent_extra_args) = handler.parent_extra_args {
                            self.extra_args = parent_extra_args;
                        }
                        
                        self.trap_sp = handler.parent_trap_sp;
                        self.current_handler = handler.parent_handler;
                        
                        let handler_closure = handler.handler_closure;
                        
                        self.stack.push(effect_val);
                        self.stack.push(cont_value);
                        self.accu = handler_closure;
                        
                        let closure_block = handler_closure.as_block()
                            .ok_or_else(|| Error::RuntimeError("Handler is not a closure".to_string()))?;
                        
                        if unsafe { (*closure_block).tag() } != crate::value::Tag::CLOSURE {
                            return Err(Error::RuntimeError("Handler is not a closure tag".to_string()));
                        }
                        
                        let code_ptr = unsafe { (*closure_block).field(0).as_int() as usize };
                        
                        self.stack.push(Value::from_raw(self.env.as_raw()));
                        self.stack.push(Value::int(self.extra_args));
                        self.stack.push(Value::int(self.pc as isize));
                        
                        self.env = handler_closure;
                        self.extra_args = 1;
                        self.pc = code_ptr;
                    }
                }
                
                Opcode::RESUME => {
                    let cont_value = self.stack.pop()
                        .ok_or_else(|| Error::RuntimeError("Stack underflow in RESUME".to_string()))?;
                    let result_value = self.accu;
                    
                    let cont_block = cont_value.as_block()
                        .ok_or_else(|| Error::RuntimeError("RESUME expects a continuation".to_string()))?;
                    
                    if unsafe { (*cont_block).tag() } != crate::value::Tag::CONTINUATION {
                        return Err(Error::RuntimeError("RESUME expects a continuation tag".to_string()));
                    }
                    
                    let cont_index = unsafe { (*cont_block).field(0).as_int() as usize };
                    
                    if cont_index >= self.continuations.len() {
                        return Err(Error::RuntimeError("Invalid continuation index".to_string()));
                    }
                    
                    let cont = self.continuations[cont_index].clone();
                    
                    let parent_handler = EffectHandler::with_parent(
                        Value::int(0),
                        self.stack.clone(),
                        self.pc,
                        self.env,
                        self.extra_args,
                        self.trap_sp,
                        self.current_handler.clone(),
                    );
                    
                    self.stack = cont.stack;
                    self.pc = cont.pc;
                    self.accu = result_value;
                    self.env = cont.env;
                    self.extra_args = cont.extra_args;
                    self.trap_sp = cont.trap_sp;
                    
                    let mut restored_handler = cont.handler;
                    if let Some(ref mut handler) = restored_handler {
                        handler.parent_handler = Some(Box::new(parent_handler));
                    }
                    self.current_handler = restored_handler;
                }
                
                Opcode::RESUME_TERM => {
                    let cont_value = self.stack.pop()
                        .ok_or_else(|| Error::RuntimeError("Stack underflow in RESUMETERM".to_string()))?;
                    let result_value = self.accu;
                    
                    let cont_block = cont_value.as_block()
                        .ok_or_else(|| Error::RuntimeError("RESUMETERM expects a continuation".to_string()))?;
                    
                    if unsafe { (*cont_block).tag() } != crate::value::Tag::CONTINUATION {
                        return Err(Error::RuntimeError("RESUMETERM expects a continuation tag".to_string()));
                    }
                    
                    let cont_index = unsafe { (*cont_block).field(0).as_int() as usize };
                    
                    if cont_index >= self.continuations.len() {
                        return Err(Error::RuntimeError("Invalid continuation index".to_string()));
                    }
                    
                    let cont = self.continuations[cont_index].clone();
                    
                    self.stack = cont.stack;
                    self.pc = cont.pc;
                    self.accu = result_value;
                    self.env = cont.env;
                    self.extra_args = cont.extra_args;
                    self.trap_sp = cont.trap_sp;
                    self.current_handler = cont.handler;
                }
                
                Opcode::REPERFORM_TERM => {
                    let effect_val = self.accu;
                    
                    if let Some(handler) = self.current_handler.take() {
                        self.current_handler = handler.parent_handler;
                        
                        if self.current_handler.is_none() {
                            return Err(Error::RuntimeError(format!("Unhandled effect in REPERFORMTERM: {:?}", effect_val)));
                        }
                    } else {
                        return Err(Error::RuntimeError(format!("No handler in REPERFORMTERM: {:?}", effect_val)));
                    }
                    
                    let cont = Continuation::capture(
                        self.stack.clone(),
                        self.pc,
                        VAL_UNIT,
                        self.env,
                        self.extra_args,
                        self.trap_sp,
                        self.current_handler.clone(),
                    );
                    
                    let cont_index = self.continuations.len();
                    self.continuations.push(cont);
                    
                    let cont_block = self.alloc_block(heap, 1, crate::value::Tag::CONTINUATION)?;
                    unsafe {
                        (*cont_block).set_field(0, Value::int(cont_index as isize));
                    }
                    let cont_value = Value::from_block_ptr(cont_block);
                    
                    if let Some(handler) = self.current_handler.take() {
                        if let Some(parent_stack) = handler.parent_stack {
                            self.stack = parent_stack;
                        } else {
                            self.stack.clear();
                        }
                        
                        if let Some(parent_pc) = handler.parent_pc {
                            self.pc = parent_pc;
                        }
                        
                        if let Some(parent_env) = handler.parent_env {
                            self.env = parent_env;
                        }
                        
                        if let Some(parent_extra_args) = handler.parent_extra_args {
                            self.extra_args = parent_extra_args;
                        }
                        
                        self.trap_sp = handler.parent_trap_sp;
                        self.current_handler = handler.parent_handler;
                        
                        let handler_closure = handler.handler_closure;
                        
                        self.stack.push(effect_val);
                        self.stack.push(cont_value);
                        self.accu = handler_closure;
                        
                        let closure_block = handler_closure.as_block()
                            .ok_or_else(|| Error::RuntimeError("Handler is not a closure".to_string()))?;
                        
                        if unsafe { (*closure_block).tag() } != crate::value::Tag::CLOSURE {
                            return Err(Error::RuntimeError("Handler is not a closure tag".to_string()));
                        }
                        
                        let code_ptr = unsafe { (*closure_block).field(0).as_int() as usize };
                        self.env = handler_closure;
                        self.extra_args = 1;
                        self.pc = code_ptr;
                    }
                }
                
                Opcode::STOP => {
                    return Ok(self.accu);
                }
                
                _ => {
                    return Err(Error::InvalidOpcode(instr));
                }
            }
        }
    }
    
    fn call_primitive(&mut self, prim_index: usize, args: &[Value], heap: &mut Heap) -> Result<Value> {
        if prim_index >= self.primitives.functions.len() {
            return Err(Error::RuntimeError(format!("Primitive index {} out of bounds", prim_index)));
        }
        
        let prim_fn = self.primitives.functions[prim_index];
        prim_fn(self, args, heap)
    }
}

struct Opcode;

impl Opcode {
    const ACCESS_STACK0: u32 = 0;
    const ACCESS_STACK1: u32 = 1;
    const ACCESS_STACK2: u32 = 2;
    const ACCESS_STACK3: u32 = 3;
    const ACCESS_STACK4: u32 = 4;
    const ACCESS_STACK5: u32 = 5;
    const ACCESS_STACK6: u32 = 6;
    const ACCESS_STACK7: u32 = 7;
    const ACCESS_STACK: u32 = 8;
    
    const PUSH: u32 = 9;
    const PUSH_ACCESS_STACK0: u32 = 10;
    const PUSH_ACCESS_STACK1: u32 = 11;
    const PUSH_ACCESS_STACK2: u32 = 12;
    const PUSH_ACCESS_STACK3: u32 = 13;
    const PUSH_ACCESS_STACK4: u32 = 14;
    const PUSH_ACCESS_STACK5: u32 = 15;
    const PUSH_ACCESS_STACK6: u32 = 16;
    const PUSH_ACCESS_STACK7: u32 = 17;
    const PUSH_ACCESS_STACK: u32 = 18;
    
    const POP: u32 = 19;
    const ASSIGN_STACK: u32 = 20;
    
    const ACCESS_ENVIRONMENT1: u32 = 21;
    const ACCESS_ENVIRONMENT2: u32 = 22;
    const ACCESS_ENVIRONMENT3: u32 = 23;
    const ACCESS_ENVIRONMENT4: u32 = 24;
    const ACCESS_ENVIRONMENT: u32 = 25;
    
    const PUSH_ACCESS_ENVIRONMENT1: u32 = 26;
    const PUSH_ACCESS_ENVIRONMENT2: u32 = 27;
    const PUSH_ACCESS_ENVIRONMENT3: u32 = 28;
    const PUSH_ACCESS_ENVIRONMENT4: u32 = 29;
    const PUSH_ACCESS_ENVIRONMENT: u32 = 30;
    
    const PUSH_RETURN_ADDRESS: u32 = 31;
    const APPLY: u32 = 32;
    const APPLY1: u32 = 33;
    const APPLY2: u32 = 34;
    const APPLY3: u32 = 35;
    const APP_TERM: u32 = 36;
    const APP_TERM1: u32 = 37;
    const APP_TERM2: u32 = 38;
    const APP_TERM3: u32 = 39;
    const RETURN: u32 = 40;
    const RESTART: u32 = 41;
    const GRAB: u32 = 42;
    const CLOSURE: u32 = 43;
    const CLOSURE_REC: u32 = 44;
    const OFFSET_CLOSURE_M2: u32 = 45;
    const OFFSET_CLOSURE0: u32 = 46;
    const OFFSET_CLOSURE2: u32 = 47;
    const OFFSET_CLOSURE: u32 = 48;
    
    const C_CALL_1: u32 = 49;
    const C_CALL_2: u32 = 50;
    const C_CALL_3: u32 = 51;
    
    const GET_GLOBAL: u32 = 52;
    const PUSH_GET_GLOBAL: u32 = 53;
    const GET_GLOBAL_FIELD: u32 = 54;
    const SET_GLOBAL: u32 = 55;
    
    const ATOM0: u32 = 56;
    const ATOM: u32 = 57;
    const PUSH_ATOM0: u32 = 58;
    const PUSH_ATOM: u32 = 59;
    
    const MAKE_BLOCK: u32 = 60;
    const MAKE_BLOCK1: u32 = 61;
    const MAKE_BLOCK2: u32 = 62;
    const MAKE_BLOCK3: u32 = 63;
    
    const MAKE_FLOAT_BLOCK: u32 = 64;
    
    const GET_FIELD0: u32 = 65;
    const GET_FIELD1: u32 = 66;
    const GET_FIELD2: u32 = 67;
    const GET_FIELD3: u32 = 68;
    const GET_FIELD: u32 = 69;
    
    const GET_FLOAT_FIELD: u32 = 70;
    
    const SET_FIELD0: u32 = 71;
    const SET_FIELD1: u32 = 72;
    const SET_FIELD2: u32 = 73;
    const SET_FIELD3: u32 = 74;
    const SET_FIELD: u32 = 75;
    
    const SET_FLOAT_FIELD: u32 = 76;
    
    const VECTOR_LENGTH: u32 = 77;
    const GET_VECTOR_ITEM: u32 = 78;
    const SET_VECTOR_ITEM: u32 = 79;
    
    const GET_STRING_CHAR: u32 = 80;
    const SET_STRING_CHAR: u32 = 81;
    
    const BRANCH: u32 = 82;
    const BRANCH_IF: u32 = 83;
    const BRANCH_IF_NOT: u32 = 84;
    const SWITCH: u32 = 85;
    const BOOLEAN_NOT: u32 = 86;
    
    const PUSH_TRAP: u32 = 87;
    const POP_TRAP: u32 = 88;
    const RAISE: u32 = 89;
    
    const CHECK_SIGNALS: u32 = 90;
    
    const CONSTANT_INT: u32 = 91;
    const PUSH_CONSTANT_INT: u32 = 92;
    const NEGATE_INTEGER: u32 = 93;
    const ADD_INTEGER: u32 = 94;
    const SUBTRACT_INTEGER: u32 = 95;
    const MULTIPLY_INTEGER: u32 = 96;
    const DIVIDE_INTEGER: u32 = 97;
    const MODULO_INTEGER: u32 = 98;
    const AND_INTEGER: u32 = 99;
    const OR_INTEGER: u32 = 100;
    const XOR_INTEGER: u32 = 101;
    const LOGICAL_SHIFT_LEFT_INTEGER: u32 = 102;
    const LOGICAL_SHIFT_RIGHT_INTEGER: u32 = 103;
    const ARITHMETIC_SHIFT_RIGHT_INTEGER: u32 = 104;
    
    const EQUAL: u32 = 105;
    const NOT_EQUAL: u32 = 106;
    const LESS_THAN_INTEGER: u32 = 107;
    const LESS_EQUAL_INTEGER: u32 = 108;
    const GREATER_THAN_INTEGER: u32 = 109;
    const GREATER_EQUAL_INTEGER: u32 = 110;
    
    const OFFSET_INTEGER: u32 = 111;
    const OFFSET_REF: u32 = 112;
    const IS_INTEGER: u32 = 113;
    
    const GET_METHOD: u32 = 114;
    const BRANCH_EQUAL: u32 = 115;
    const BRANCH_NOT_EQUAL: u32 = 116;
    const BRANCH_LESS_THAN_INTEGER: u32 = 117;
    const BRANCH_LESS_EQUAL_INTEGER: u32 = 118;
    const BRANCH_GREATER_THAN_INTEGER: u32 = 119;
    const BRANCH_GREATER_EQUAL_INTEGER: u32 = 120;
    
    const UNSIGNED_LESS_THAN_INTEGER: u32 = 121;
    const UNSIGNED_GREATER_EQUAL_INTEGER: u32 = 122;
    
    const BRANCH_UNSIGNED_LESS_THAN_INTEGER: u32 = 123;
    const BRANCH_UNSIGNED_GREATER_EQUAL_INTEGER: u32 = 124;
    
    const GET_PUBLIC_METHOD: u32 = 125;
    const GET_DYNAMIC_METHOD: u32 = 126;
    
    const STOP: u32 = 127;
    const C_CALL_4: u32 = 128;
    const C_CALL_5: u32 = 129;
    const C_CALL_N: u32 = 130;
    const EVENT: u32 = 128;
    const BREAK: u32 = 129;
    
    const RERAISE: u32 = 130;
    const RAISE_NOTRACE: u32 = 131;
    
    const GET_STRING_CHAR_UNSAFE: u32 = 132;
    const GET_VECTOR_ITEM_UNSAFE: u32 = 133;
    const SET_VECTOR_ITEM_UNSAFE: u32 = 134;
    const GET_METHOD_LABEL: u32 = 135;
    
    const CONSTANT0: u32 = 136;
    const CONSTANT1: u32 = 137;
    const CONSTANT2: u32 = 138;
    const CONSTANT3: u32 = 139;
    
    const PUSH_CONSTANT0: u32 = 140;
    const PUSH_CONSTANT1: u32 = 141;
    const PUSH_CONSTANT2: u32 = 142;
    const PUSH_CONSTANT3: u32 = 143;
    
    const PERFORM: u32 = 147;
    const RESUME: u32 = 148;
    const RESUME_TERM: u32 = 149;
    const REPERFORM_TERM: u32 = 150;
}

