%option debug warn

%{
#include <stdio.h>
#include "exp1.tab.h"
%}

DIGIT    [0-9]

%%
{DIGIT}+   		return INTNUMBER;
Nutzer|Bot 		return PROPN;
der|ein 		return DETNOM;
Hund|Tisch|Baum|Fisch 	return OBJECT;
interessierte|großer|kleiner	 return ADJ;
ist			 return V;
Was|Wer			return QWORD;
[ \t]+        putchar( ' ' );
[ \t]+$
%%