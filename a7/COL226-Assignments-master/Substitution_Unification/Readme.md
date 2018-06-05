# Assignment 4: Terms, substitutions, unification.
#### *Submitted by - Suyash Agrawal (2015CS10262)*

### Assumptions
* Size is sum of all the atoms in the term
* Height of a Variable or Symbol of zero arity is 1

### Design Descisions
* Signature is of form Sym*int list
* vars return a set containing string representation of type Var
* substituion is modelled as a function : variable -> term , as this made composing substitutions fast and easy.

### Running Testcases
Just copy and paste the code written in "testcases.ml" file in the top level ocaml interpreter.
