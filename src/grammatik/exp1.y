%{
#include <stdio.h>
#include <string.h>
#include "lex.yy.h"
#include "../common.h"

void yyerror(const char *str)
{
        fprintf(stderr,"error: %s\n",str);
}
 
int yywrap()
{
        return 1;
} 

/* 
  convenience function for input/output to the parser module 
  returns an array of tagged words  
*/
struct stWord** parser_wrap(char* sLineInput)
{
      YY_BUFFER_STATE hdlParseBuf;
      struct stWord* spTmp;

      /* parse input*/
      hdlParseBuf=yy_scan_string (sLineInput);
      yyparse();
      yy_delete_buffer(hdlParseBuf);
      return(0);
} 

  
%}

/*%define parse.trace true
%define parse.error verbose
%define api.value.type {char *}*/

%union {char* str;
  double val;
}

%token <str>INTNUMBER <str>PROPN <str>DETNOM <str>OBJECT <str>ADJ <str>V <str>QWORD <str>NUNKNOWN
%type <str> question
%type <str> statement
%type <str> vp
%type <str> np
%type <str> nom

%%
start: question | statement;
question: QWORD vp np {printf("QWORD %s\n",$1);};
statement: np vp np {printf("Statement\n");};
np: 
  DETNOM nom {printf("DETNOM %s\n",$1);}
  | PROPN {printf("PROPN %s\n",$1);};
nom: 
  ADJ nom 
  | OBJECT {printf("OBJECT %s\n",$1);}
  | NUNKNOWN {printf("NUNKNOWN %s\n",$1);};
vp: V {printf("V %s\n",$1);};
%%
