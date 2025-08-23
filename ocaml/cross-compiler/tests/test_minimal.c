// Minimal test without libc
int _start() {
    // Exit with code 42
    asm("mov $60, %rax");  // sys_exit
    asm("mov $42, %rdi");  // exit code  
    asm("syscall");
}