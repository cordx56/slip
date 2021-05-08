# slip
slip - Small LIsP compiler project

## Run
### Print test code LLVM IR
```bash
$ cargo run test.slip
```
### Run test code
```bash
$ cargo run test.slip | lli -
```

## Language specification
### BNF
```
<program>    ::= (<multispace>* <expression> <multispace>*)*
<expression> ::= <atom> | <list>
<list>       ::= "(" <expression> (<multispace>+ <expression>)* ")" | "(" <multispace>* ")"
<atom>       ::= <identifier> | <constant>
<constant>   ::= <boolean> | <number> | <string>
<boolean>    ::= "#t" | "#f"
```
