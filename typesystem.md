
# Explaining the type system

To each circuit is associated a type `t`. This type is a pair 
of positive integers written `n -> m`. The first part represents 
the number of inputs of the given circuit, and the second part 
the nuber of outputs.

## From type inference to linear system 

To be able to infer the value of a polymorphic circuit (such as `id`),
type variables are added, they are unknown variables.

The type inference system will then find all the constraints that 
the types should satisfy for the expression to be typeable.

The thing to notice is that theses equations are simple, they are 
linear equations !

## Solving the linear system

Four cases can be distinguished 

1. The system has a unique solution, and this solution is composed of 
natrual numbers
2. The system has a unique solution, but one of the values is not 
a natural number
3. The system has no solutions
4. The system has multiple solutions

Thoses are obviously the only cases. To each case can be associated a 
response of the typing system :

1. It gives the only type possible 
2. The expression has no type, and the variable that is not a natural 
number can be shown as a place where the expression is ill-typed.
3. The expression has no type, and with a gaussian elimination, we can 
find the variables that are not typeable
4. The expression has a type, but there are not enough constraints
to decide among the infinite number of types that it can have. The 
type system can show a vector in the kernel that can « move freely »
(parts of the expression that are to move together, but dont influence 
the other parts of the expression).


## Example 

For instance, the expression 

id ||| id

Has 6 variables.

* `id : a1 -> a2`
* `id : a3 -> a4`
* `(id ||| id) : a5 -> a6`

The constraints are

* `a5 = a1 + a3`
* `a6 = a2 + a4` 
* `a1 = a2`
* `a3 = a4`

The matrix is therefore

```
1  0 1  0 -1  0
0  1 0  1  0 -1
1 -1 0  0  0  0
0  0 1 -1  0  0
```

And the other part of the equation is 
the vector

```
0
0
0
0
```

The rank of the matrix is less than `6` so there is 
more than one solution. We can find a vector in the 
kernel :

```
1 1 0 0 1 1
```

Which can be converted back to 

```
a1 - a5 = 0
a2 - a6 = 0
a1 - a2 = 0
```

The message could then be something like « thoses variables
are free, fix one of them ». 

Warning: fixing one of them is not always a direct solution,
as the kernel can have a large dimension. A solution 
could be to give a basis of the kernel ... 

# Detailed algorithm

1. Construct the matrix associated with the equations 
2. If the number of columns is greater than the number of lines, find a basis of the kernel
3. Otherwise, do a gaussian elimination on the lines.
    1. If a null column is found during the gaussian elimination : then find a basis of the kernel (a variable is unused)
    2. Check that every row below the number of columns (bottom part of the matrix) contains zeros, and that the 
       second member does as well.
    3. Resolve (upper resolution) 
