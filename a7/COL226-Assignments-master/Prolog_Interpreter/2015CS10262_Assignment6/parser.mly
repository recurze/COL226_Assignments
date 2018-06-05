%{
  open Structure
  open Printf
%}

%token END
%token SEP
%token <string> VAR CONS
%token COMMA
%token O_PAREN
%token C_PAREN
%token O_SQ
%token C_SQ
%token OR
%token EOL
%token WHITESPACE
%token EOF
%token EQUAL
%token NOT_EQUAL
%token NOT

%nonassoc EQUAL
%nonassoc NOT_EQUAL
%start main
%start goal
%type <Structure.program> main
%type <Structure.goal> goal
%%

main:
  | EOF                    { [] }
  | clause main            { ($1)::($2) }
;

goal:
  | atom_list END          { $1 }

clause:
  | atom END               { Fact(Head($1)) }
  | atom SEP atom_list END     { Rule(Head($1),Body($3)) }
;

atom:
  | NOT O_PAREN atom C_PAREN               { Atom(Sym("$not"),[Node($3)]) }
  | CONS O_PAREN term_list C_PAREN         { Atom(Sym($1),$3) }
  | term EQUAL term                        { Atom(Sym("$eq"),[$1;$3]) }
  | term NOT_EQUAL term                    { Atom(Sym("$neq"),[$1;$3]) }
  | O_PAREN atom C_PAREN                   { $2 }
;

atom_list:
  | atom                   { [$1] }
  | atom COMMA atom_list   { ($1)::($3) }
;

term_list:
  | term                   { [$1] }
  | term COMMA term_list   { ($1)::($3) }
;

term:
  | VAR                    { Var($1) }
  | CONS                   { Cons($1) }
  | atom                   { Node($1) }
  /*| O_PAREN term C_PAREN                   { $2 }  */
;