%{
#include <stdio.h>
#include <string.h>
 
void yyerror(const char *str)
{
        fprintf(stderr,"error: %s\n",str);
}
 
int yywrap()
{
        return 1;
} 
  
int main()
{
        return yyparse();
} 

%}

%token INTNUMBER PROPN DETNOM OBJECT ADJ V QWORD
/*%define parse.trace true
%define parse.error verbose*/
%debug
%verbose

%%
start: question | statement {};
question: QWORD vp np {printf("QWORD %s\n",$1);};
statement: np vp np {printf("Statement\n");};
np: 
  DETNOM nom {printf("DETNOM %s\n",$1);}
  | PROPN {printf("PROPN %s\n",$1);};
nom: 
  ADJ nom 
  | OBJECT {printf("OBJECT %s\n",$1);};
vp: V {printf("V %s\n",$1);};
%%
