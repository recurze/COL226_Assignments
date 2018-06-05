/* File parser.mly */
%token <int> INT
%token TIMES DIV
%token EOL
%right TIMES DIV
%start main             /* the entry point */
%type <int> main
%%
main:
    subexp EOL                { $1 }
;
subexp:
    unit                     { $1 }
    | unit TIMES subexp         { $1 * $3 }
unit:
    base            {$1}
    | base DIV unit {$1 / $3}
;
base:
    INT   {$1}
;