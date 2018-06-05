%{
  open Types
  open Printf
%}

%token SEP_

%token O_PAREN_
%token C_PAREN_

%token IF_
%token THEN_
%token ELSE_

%token LET_
%token FUNC_
%token ARROW_

%token <string> VAR_
%token <int> INTEGER_
%token <bool> BOOLEAN_

%token OR_
%token AND_
%token EQUAL_
%token ADD_
%token SUB_
%token MUL_
%token LESS_
%token GRT_
%token LESS_EQ_
%token GRT_EQ_

%token NOT_EQUAL_
%token NOT_

%token WHITESPACE_
%token EOF

%left LESS_EQ_ GRT_EQ_ LESS_ GRT_
%left OR_ AND_
%left ADD_ SUB_
%left MUL_
%nonassoc EQUAL_
%nonassoc NOT_EQUAL_

%start main
%type <Types.program> main
%type <Types.expression> expression
%type <Types.expression> function
%%

main:
  | EOF                    { [] }
  | expression SEP_ main            { ($1)::($3) }
;

expression:
  | LET_ VAR_ EQUAL_ expression     {BIND(Var $2, $4)}
  | INTEGER_                        {INTEGER($1)}
  | BOOLEAN_                        {BOOL($1)}
  | VAR_                            {VAR( Var $1 )}
  | expression MUL_ expression      {MUL($1,$3)}
  | expression ADD_ expression      {ADD($1,$3)}
  | expression SUB_ expression      {SUB($1,$3)}
  | expression EQUAL_ expression    {EQUAL($1,$3)}
  | expression OR_ expression       {OR($1,$3)}
  | expression AND_ expression      {AND($1,$3)}
  | expression LESS_ expression     {LESS($1,$3)}
  | expression GRT_ expression      {GRT($1,$3)}
  | expression LESS_EQ_ expression  {LESS_EQ($1,$3)}
  | expression GRT_EQ_ expression   {GRT_EQ($1,$3)}
  | function                        { $1 }
  | IF_ O_PAREN_ expression C_PAREN_ THEN_ O_PAREN_ expression C_PAREN_ ELSE_ O_PAREN_ expression C_PAREN_ { IF_THEN_ELSE ($3,$7,$11) }
  | VAR_ O_PAREN_ expression C_PAREN_ { APPLY(VAR( Var $1 ),$3) }
  | function O_PAREN_ expression C_PAREN_ { APPLY($1,$3) }
  | O_PAREN_ expression C_PAREN_    { $2 }
;

function:
  | FUNC_ VAR_ ARROW_ O_PAREN_ expression C_PAREN_      {FUNC( Var $2 , $5)}
;