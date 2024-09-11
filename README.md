# Cerium

A general-purpose programming language that aims to be the most sane

## Roadmap

- Language
    - Syntax 
        - [x] Add support for Function Declarations
        - [x] Add support for Function Calls
        - [x] Add support for Function Parameters Access
        - [x] Add support for Variable Declarations
            - [x] Local Variable Declarations
            - [x] Global Variable Declarations
        - [x] Add support for Variable Access
            - [x] Local Variable Access
            - [x] Global Variable Access

    - Control Flow
        - [ ] Add support for Conditionals
            - [ ] If Condition 
        - [ ] Add support for Loops
            - [ ] While Loop

    - Data 
        - [x] Add support for Integers
        - [x] Add support for Floats
        - [x] Add support for Booleans
        - [x] Add support for Pointers
        - [x] Add support for Functions
        - [ ] Add support for Structures
        - [ ] Add support for Enumerations 
        - [ ] Add support for Unions 

    - Operations
        - [x] Add support for Unary Operations
            - [x] Negation
            - [x] Boolean Not
            - [x] Bitwise Not
        - [ ] Add support for Binary Operations
            - [x] Addition
            - [x] Subtraction
            - [x] Multiplication
            - [x] Division
            - [ ] Comparison
                - [ ] Less Than
                - [ ] Greater Than
                - [x] Equals To
            - [ ] Bitwise Shfting
                - [ ] Shift Left 
                - [ ] Shift Right 

- Standard Library
    - [ ] Make a robust startup code
    - [ ] Let the startup code be compiled automatically with any provided code
    - [ ] Add support for accessing the Standard Library
    - [ ] Make a System Call API for various operating systems
        - [ ] Make a System Call API for Linux
        - [ ] Make a System Call API for Windows

- Cross Compilation
    - [ ] Allow passing a Triple Target via Command Line Arguments
    - [ ] Add support for compiling to various Assembly Outputs
        - [ ] Add support for compiling to Aarch64 Assembly
        - [x] Add support for compiling to x86_64 Assembly

- Inline Assembly
    - [x] Add support for outputing Assembly
    - [x] Add support for input constraints
    - [x] Add support for output constraints
