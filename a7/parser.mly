%{
    open Solve
%}

%token EORF
%token EOL
%token EOF
%token Rule_def
%token Comma
%token Semi_Colon
%token Whitespace
%token Open_Sq
%token Open_Brace
%token Close_Brace
%token Close_Sq

%token Pipe
%token Underscore
%token <string> V Constructor Const

%start program
%type <Solve.program> program

%start goal
%type <Solve.goal> goal

%%

program:
    |   EOF { [] }
    |   clause program {($1)::($2)}
;

goal:
    |   term_list EORF { $1 }
    ;

clause:
    |   term EORF   { Fact($1) }
    |   term Rule_def term_list EORF { Rule($1, $3) }
;

term:
    |   V { V($1) }
    |   Const { Const($1) }
    |   Constructor { Node(S($1, 0), []) }
    |   Constructor Open_Brace term_list Close_Brace { Node(S($1, 1), $3) }

    |   piped  { $1 }
    |   Open_Sq term_list Close_Sq { Node(S("List", 1), $2) }
    ;

piped:
    |   Open_Sq term Pipe term_list Close_Sq { Node(S("Pipe", 2), $2::$4) }
    ;

term_list:
    |   term { [$1] }
    |   term Comma term_list { ($1)::($3) }
;
