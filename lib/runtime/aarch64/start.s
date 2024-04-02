.global _start
_start:
    // Call the main function
    bl main

    // Exit no matter what the main function does
    mov x0, #0
    mov w8, #93
    svc #0
