## Assignment 3: Expression Evaluator

#### Submitted by: Suyash Agrawal (2015CS10262)

### Precedence Rules:
1. Div = Mul > Add = Sub (Took standard precedence order as with all mathematical computation tools like bc)
2. And > Or
3. Unary operators have most precedence
4. All operators are left associative

### Design Decisions
1. Arithmetic operators are not defined for booleans (i.e. no casting is performed)
2. Boolean operators on integers are not defined (i.e. integers are not automatically cast to booleans)

### Usage
1. Genearate executable by running make
2. Execute "./calc" without any command line args.
3. Enter any valid expression (like "5*2" ) and press enter.

