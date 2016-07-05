# Compiling to diagrams

Definition of a langage to express circuit diargams
and the compilator associated.

## Dependencies

This program needs 

* A recent version of the ocaml compiler 
* GNU Make 

## Building and running 

To build the project and create the `comp` executable run 

```
make comp 
```

To run the tests use 

```
make tests
```

To compile and run the circuits 

```
make circuits 
```

## Syntax overview 

The syntax to describe circuits is pretty simple, and has three different
type of expressions. First of all, a circuit is represented by an expression,
and expressions can be _composed_ to create bigger circuits, this is what 
the base syntax allows for. Even if every circuit can be expressed in that way,
it is often convenient to use variables to name inputs and outputs of circuits,
and use the same variable across the construction, this is what the variable binding 
is about. Finally, sometimes the variables are to be linked together, an never 
used again (linking port of circuits), this is the last part.

### Base syntax

The basic circuits are the following ones

1. The identity circuit `n` which is the identity for n wires
2. The fork operator `f` and it's dual the join operator `j`
3. The disconnected wire `_|_` and the forget `w` 
4. The trace operator
5. The switch operator

### Linking 

...

# Things that would be done differently 

1. Separate labelling from meaning : a label is just a string
2. Do all the stuff directly on liDAGS rather than the PTG version
3. Use name translation rather than a global ref count (leads to mistakes)
