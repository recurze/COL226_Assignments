%{
	#include "test.tab.h"
%}

%%
"+" { printf("PLUS\n"); return ADD;}
"-" { printf("MINUS\n"); return SUB;}
"*" { printf("MULTIPLY\n"); return MUL;}
"/" { printf("DIVIDE\n"); return DIV;}
[0-9]+.?[0-9]* {printf("NUMBER - %s\n",yytext); yylval= atof(yytext); return NUMBER;}
"sqrt" { printf("SQRT\n"); }
"pow" { printf("POW\n"); }
"log" { printf("LOG\n"); }
. { printf("IGNORE\n"); }
\n {return EOL; }

%%;