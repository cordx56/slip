# slip
slip - Small LIsP compiler project

## Run
### Install LLVM 11 using llvmenv
- install cmake, make, ninja, g++/clang++
- `cargo install llvmenv`
- `llvmenv init`
- `llvmenv build-entry 11.0.0`
- `set -x LLVM_SYS_110_PREFIX (llvmenv prefix)`
### Run test code
```bash
$ cargo run test.slip
```
### Print test code LLVM IR
```bash
$ cargo run test.slip --llvmir
```

## Language specification
### BNF
```
<program>    ::= (<multispace>* <expression> <multispace>*)*
<expression> ::= <atom> | <list>
<list>       ::= "(" <multispace>* <expression> (<multispace>+ <expression>)* <multispace>* ")" | "(" <multispace>* ")"
<atom>       ::= <identifier> | <constant>
<constant>   ::= <number> | <string>
```
