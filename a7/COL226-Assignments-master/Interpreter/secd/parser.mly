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
%type <Types.opcode list> main
%type <Types.opcode list> expression
%type <Types.opcode list> function
%%

main:
  | EOF                    { [] }
  | expression SEP_ main            { ($1)@($3) }
;

expression:
  | LET_ VAR_ EQUAL_ expression      {($4)@[LET( Var($2) )]}
  | INTEGER_                        {[INTEGER($1)]}
  | BOOLEAN_                        {[BOOL($1)]}
  | VAR_                            {[ACCESS( Var($1) )]}
  | expression MUL_ expression      {($1)@($3)@[MUL]}
  | expression ADD_ expression      {($1)@($3)@[ADD]}
  | expression SUB_ expression      {($1)@($3)@[SUB]}
  | expression EQUAL_ expression    {($1)@($3)@[EQUAL]}
  | expression OR_ expression       {($1)@($3)@[OR]}
  | expression AND_ expression      {($1)@($3)@[AND]}
  | expression LESS_ expression     {($1)@($3)@[LESS]}
  | expression GRT_ expression      {($1)@($3)@[GRT]}
  | expression LESS_EQ_ expression  {($1)@($3)@[LESS_EQ]}
  | expression GRT_EQ_ expression   {($1)@($3)@[GRT_EQ]}
  | function                        { $1 }
  | IF_ O_PAREN_ expression C_PAREN_ THEN_ O_PAREN_ expression C_PAREN_ ELSE_ O_PAREN_ expression C_PAREN_ {($3)@[ ( IF_THEN_ELSE ( ($7) , ($11) ) ) ]}
  | VAR_ O_PAREN_ expression C_PAREN_ { ($3)@([ACCESS( Var($1) )])@[APPLY] }
  | function O_PAREN_ expression C_PAREN_ { ($3)@($1)@[APPLY] }
  | O_PAREN_ expression C_PAREN_    { $2 }
;

function:
  | FUNC_ VAR_ ARROW_ O_PAREN_ expression C_PAREN_      {[ CLOSURE( [LET( Var($2) )] @ ($5) @ [RETURN] ) ]}
;