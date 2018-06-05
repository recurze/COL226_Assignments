%{
   open Typefile
%}

/* Integer type token */
%token <int> INT
/* Boolean type tokens */
%token TRUE FALSE
/* token for variable */
%token <string> VAR
/* Unary operator tokens */
%token ABS NOT MINUS 
/* Binary logical operator token */
%token OR AND
/* Binary integer operator token */
%token ADD SUB MUL DIV MOD LESS GRT LEQ GEQ EQUAL
/* Parenthesis */
%token LPAREN RPAREN
/* End of line */
%token EOL

%left LESS
%left LEQ
%left GRT
%left GEQ
%left EQUAL
%left OR
%left AND
%left SUB ADD
%left MUL DIV MOD
%left EXP
%start main             /* the entry point */
%type <Typefile.parse_tree> main
%%

main:
    expr EOL                  { $1 }
;
expr:
    | subexp                  { Elepar("expr",$1) }
    | expr ADD subexp         { Bipar("expr",Add,$1,$3) }
    | expr SUB subexp         { Bipar("expr",Sub,$1,$3) }
    | expr OR subexp          { Bipar("expr",Or,$1,$3) }
    | expr LESS subexp        { Bipar("expr",Less,$1,$3) }
    | expr GRT subexp         { Bipar("expr",Grt,$1,$3) }
    | expr LEQ subexp         { Bipar("expr",Leq,$1,$3) }
    | expr GEQ subexp         { Bipar("expr",Geq,$1,$3) }
    | expr EQUAL subexp       { Bipar("expr",Equal,$1,$3) }
;

subexp:
    | unary                   { Elepar("subexp",$1) }
    | subexp MUL unary        { Bipar("subexp",Mul,$1,$3) }
    | subexp AND unary        { Bipar("subexp",And,$1,$3) }
    | subexp DIV unary        { Bipar("subexp",Div, $1,$3) }
    | subexp MOD unary        { Bipar("subexp",Mod,$1,$3) }
;

unary:
    | base                    { Elepar("unary",$1) }
    | ABS unary               { Unipar("unary",Abs,$2) }
    | NOT unary               { Unipar("unary",Not,$2) }
    | MINUS unary             { Unipar("unary",Minus,$2) }
;

base:
    | LPAREN expr RPAREN      { Elepar("base",$2) }
    | SUB INT                 { Elepar("base",Base(Int(-1 * $2))) }
    | INT                     { Elepar("base",Base(Int($1))) }
    | TRUE                    { Elepar("base",Base(Bool(true))) }
    | FALSE                   { Elepar("base",Base(Bool(false))) }
    | VAR                     { Elepar("base",Base(Var($1))) }
;

