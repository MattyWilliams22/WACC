# WACC Compiler

This project is a compiler for the **WACC (While And Control Command)** programming language. Developed as part of a group project during the spring term of our second year at university, the compiler was designed with a modular structure, including a front-end, back-end, and additional extensions.

## Features

- **Front-End**:
  - Lexical analysis.
  - Syntax analysis.
  - Semantic analysis to validate WACC programs.

- **Back-End**:
  - Intermediate representation (IR) generation.
  - Code generation for ARM architecture.

- **Extensions**:
  - Additional features to extend WACC functionality:
    - Peephole optimisations
    - Standard library functions
    - Control flow analysis

## Repository Structure

- **`src/main/scala/wacc/`**: Core source code for the compiler, organized into front-end, back-end, and extensions.
- **`wacc_examples/`**: Sample WACC programs for testing the compiler.
- **`src/test/wacc/`**: Tests to verify the correctness of implemented features.

## Technologies Used

- **Scala**: Primary language for compiler implementation.
- **ARM Assembly**: Target architecture for code generation.

## Getting Started

### Prerequisites

- Java Development Kit (JDK 8 or later).
- A Linux or macOS system for ARM code emulation (via QEMU).

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/MattyWilliams22/WACC.git
   cd WACC
   ```
2. Build the project:
   ```bash
   make
   ```

### Testing 

Tests can be ran via `scala-cli test .`.
