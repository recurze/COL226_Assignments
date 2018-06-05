Tokenizer for a simple arithmetic and boolean calculation language.

Design:
1. All token should preferably be delimited by a space character to avoid ambiguity in parsing (like 3-3 can be Integer(3), Integer(-3) or Integer(3) Operator(-) Integer(3) ).
2. '-' 'div' 'mod' 'if' 'then' 'else' should have spaces around them to be correctly interpreted or must be parenthesized. 
3. Though this is not compulsory and expression like "if(33>=37)then" are correctly parsed despite lack of delimitation by space.
4. This design decision was taken by taking into account the ambiguity cause by lack of delimiter(space).

Usage:
use "make" command to build the source code.
use "./scanner <input_file_name>" to print tokens parsed from the input file.

Note: Tokens are printed to stdout stream.
