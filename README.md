# Cerium

A general purpose programming language that aims to be sane and at least functional

## Why would I need this?

I don't think you would need it, Zig and C and Rust and others would do the same thing and maybe better, but at least it is functional

It exists as a "just works" programming language, no need to be the "ultimate, perfect, secure, most maintainable", it is cool that there are some programming languages like that, but that's not always why people use programming languages, the main idea is for a tool that provides easy access to make binary files (or programs) without having to work with writing Assembly by hand (or bytecode in the case of virtual machines), Cerium's goal is to achieve main and required ideas of a programming language, a way to write human readable text that gets translated to machine readable code

If you like it give it a star and try it yourself, If you don't like it then I couldn't care less

## What did you currently achieve and what do you look for?

- Semantics
    - [ ] Add support for Function Declarations
        - [ ] Local Function Declarations
        - [x] Global Function Declarations
    - [ ] Add support for Function Calls
    - [ ] Add support for Function Parameters Access
    - [ ] Add support for Variable Declarations
        - [x] Local Variable Declarations
        - [ ] Global Variable Declarations
    - [ ] Add support for Variable Access
        - [x] Local Variable Access
        - [ ] Global Variable Access
    - [ ] Add support for Unary Operations
        - [x] Negation
        - [ ] Boolean Not
        - [ ] Bitwise Not
    - [ ] Add support for Binary Operations
        - [x] Addition
        - [x] Subtraction
        - [x] Multiplication
        - [x] Division
        - [ ] Comparison
            - [ ] Less Than
            - [ ] Greater Than
            - [ ] Equals To
        - [ ] Bitwise Shfting
    - [ ] Add support for Pointer Operations
        - [ ] Get Pointer
        - [ ] Access Pointer
            - [ ] Write to Pointer
            - [ ] Read from Pointer

- Standard Library
    - [ ] Make a robust startup code (maybe called `runtime`)
    - [ ] Let the startup code be compiled automatically with any provided code
    - [ ] Add support for accessing the Standard Library
    - [ ] Make a System Call API for various operating systems
        - [ ] Make a System Call API for Linux
        - [ ] Make a System Call API for Windows

- Cross Compilation
    - [ ] Allow passing a Triple Target via Command Line Arguments
    - [x] Add support for compiling to various Assembly Outputs
        - [x] Add support for compiling to Aarch64 Assembly
        - [x] Add support for compiling to x86_64 Assembly

- Inline Assembly
    - [x] Add support for outputing Assembly
    - [ ] Add support for input constraints
    - [ ] Add support for output constraints

