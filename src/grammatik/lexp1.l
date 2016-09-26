%option debug warn

%{
#include <stdio.h>
#include "exp1.tab.h"

%}

DIGIT		[0-9]
LETTER		[a-zA-ZäüöÄÖÜ]

%%
{DIGIT}+   		{yylval.str = strdup(yytext); return INTNUMBER;}
Nutzer|Bot 		{yylval.str = strdup(yytext); return PROPN;}
der|ein 		{yylval.str = strdup(yytext); return DETNOM;}
Hund|Tisch|Baum|Fisch 	{yylval.str = strdup(yytext); return OBJECT;}
interessierte|großer|kleiner	 {yylval.str = strdup(yytext); return ADJ;}
ist			 {yylval.str = strdup(yytext); return V;}
Was|Wer			{yylval.str = strdup(yytext); return QWORD;}
{LETTER}+		{yylval.str = strdup(yytext); return NUNKNOWN;}
[ \t]+        putchar( ' ' );
[ \t]+$
%%