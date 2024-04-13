.global _start
_start:
    // Call the main function
    call main

    // Exit no matter what the main function does
    mov $60, %rax
    mov $0, %rdi
    syscall
