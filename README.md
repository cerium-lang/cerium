# Cerium

A general-purpose programming language that aims to be the most sane

## Roadmap

- Language
    - Control
        - [x] Add support for Conditionals
        - [x] Add support for Loops
        - [ ] Add support for Switch

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
            - [x] Boolean Operations
                - [x] Not
            - [x] Bitwise Operations
                - [x] Not
        - [x] Add support for Binary Operations
            - [x] Addition
            - [x] Subtraction
            - [x] Multiplication
            - [x] Division
            - [x] Comparison
                - [x] Less Than
                - [x] Greater Than
                - [x] Equals To
            - [x] Bitwise Operations
                - [x] Shift
                    - [x] Shift Left
                    - [x] Shift Right
                - [x] Logical
                    - [x] And
                    - [x] Or
                    - [x] Xor

- Standard Library
    - Runners
        - [x] Make an Executable Runner for various operating systems
            - [x] Make an Executable Runner for Linux
            - [ ] Make an Executable Runner for Windows
    - System Calls
        - [ ] Make a System Call API for various operating systems
            - [ ] Make a System Call API for Linux
            - [ ] Make a System Call API for Windows

- Cross Compilation
    - [ ] Allow passing a Triple Target via Command Line Arguments
    - [ ] Compile to various targets
        - [ ] Add support for compiling to various Assembly outputs
            - [ ] Add support for compiling to Aarch64 Assembly
            - [x] Add support for compiling to x86_64 Assembly
        - [ ] Make an Assembler for those Assembly outputs

- Inline Assembly
    - [x] Add support for input constraints
    - [x] Add support for output constraints
    - [ ] Add support for clobbers
