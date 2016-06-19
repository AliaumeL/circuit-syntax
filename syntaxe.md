# Syntaxe

Formal description of the syntax that is to be parsed/lexed by the compiler.


## Symbols

Base symbols are 

* `w`
* `j`
* `f`
* `_`
* `x`
* `b` (bottom)

## Function (User Defined)

Starting with capital letter, defined using the syntax

DEFINE F a b 

a = number of inputs
b = number of outputs 

## Expression

expr := base_symbol | id int | function_symbol | IN x BEGIN expr END | BEGIN expr OUT y END | LINK [] BEGIN expr END | expr × expr | expr . expr
        | TRACE expr 


# Examples

DEFINE F 1 2
DEFINE G 1 2

LINK (x,y) (z,t) BEGIN
    (x | z) -> (F | G) -> (j -> t | j -> y) 
END

Gives 

x --- F ---- t

z --- G ---- y

t ---- z
y ---- x


