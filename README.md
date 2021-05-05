# slip
slip - Small LIsP compiler project

## Language specification
### BNF
```
<program>    ::= (<multispace>* <expression> <multispace>*)*
<expression> ::= <atom> | <list>
<list>       ::= "(" <expression> (<multispace>+ <expression>)* ")"
<atom>       ::= <identifier> | <constant>
<constant>   ::= <boolean> | <number> | <string>
<boolean>    ::= "#t" | "#f"
```
