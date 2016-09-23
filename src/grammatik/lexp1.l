%option debug warn

%{
#include <stdio.h>
#include "exp1.tab.h"

%}

DIGIT    [0-9]

%%
{DIGIT}+   		{yylval.str = strdup(yytext); return INTNUMBER;}
Nutzer|Bot 		{yylval.str = strdup(yytext); return PROPN;}
der|ein 		{yylval.str = strdup(yytext); return DETNOM;}
Hund|Tisch|Baum|Fisch 	{yylval.str = strdup(yytext); return OBJECT;}
interessierte|großer|kleiner	 {yylval.str = strdup(yytext); return ADJ;}
ist			 {yylval.str = strdup(yytext); return V;}
Was|Wer			{yylval.str = strdup(yytext); return QWORD;}
[ \t]+        putchar( ' ' );
[ \t]+$
%%