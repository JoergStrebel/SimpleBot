% et.pl - M. Covington      2003 February 12

% etu.pl - Modified for Unicode - Donald Rogers     2006 July 17
%          email: dero9753@ihug.co.nz
%          Modified to cope with comma in numbers   2006 July 20

% ET the Efficient Tokenizer
% see http://www.covingtoninnovations.com/mc/ProNTo/index.html

/*
 Funktionen des Tokenizers:
 - Erkennung von  Interpunktionszeichen
 - Erkennung von Zahlen
 - Aufspaltung des Inputs in mehrere Sätze, falls nötig
 - Umwandlung aller Inputs in Kleinschreibung

D.h der Tokenizer soll einen String nehmen und dann einen oder mehrere Sätze zurückliefern.
Jeder Satz soll eine Liste mit Atomen sein in der Form w(<Atom>), n(<Zahl>) und s('?'),s(';'),s('.'), s('!'). D.h. die Datenstruktur sieht dann so aus (am Beispiel des Satzes 'Ich gehe zu Fuß. Hilf mir!'):
[satz([w(ich),w(gehe),w(zu),w(fuß),s('.')]),satz([w(hilf),w(mir),s('!')])]
*/


/* tokenize_string(+StrIn,-LSatz).*/
tokenize_string(StrIn,Tokens) :- 
	string_chars(StrIn,Chars), 
	label_all_chars(Chars,LChars),
	collect_words(LChars,WList),
	collect_numbers(WList,WNrList),
	delete(WNrList,w(_),Tokens)
.

/* collect_words(+List of Tokens, -List with words)
*/
collect_words(LTokens, WList):-
  agg_find_all_words(LTokens,[],Erlist),
  reverse(Erlist,WList).

agg_find_all_words([],Agg,Agg):-!.

agg_find_all_words([w(_),l(H2)|T],Agg,Outputliste):-
    find_word([l(H2)|T],[],Result,Rest),
    reverse(Result,RevResult),
    agg_find_all_words(Rest,[word(RevResult)|Agg],Outputliste),
    !.

agg_find_all_words([l(H1),l(H2)|T],Agg,Outputliste):-
    find_word([l(H1),l(H2)|T],[],Result,Rest),
    reverse(Result,RevResult),
    agg_find_all_words(Rest,[word(RevResult)|Agg],Outputliste),
    !.

% not a word start - just copy the token to the output
agg_find_all_words([H|T],Agg,Outputliste):-
    agg_find_all_words(T,[H|Agg],Outputliste).


/* completes a word; LInp starts with a word 
find_word(+LInp,-LWord, -Result, -Rest)
*/
find_word([l(T)|LInp], Agg, Result, Rest):-
    find_word(LInp,[T|Agg], Result, Rest),
    !.

find_word([], LWord, LWord,[]):-!.

% not a letter, stop recursion
find_word([T|LInp], LWord, LWord,[T|LInp]).



/* collect_numbers(+List of Tokens, -List with numbers)
TODO
*/
collect_numbers(LTokens, NrList):-
    LTokens=NrList.


/* labele alle Chars in der Liste 
 label_all_chars(+InputListe,-Outputliste mit Labels)
*/

label_all_chars(InputListe,Outputliste):-
    agg_label_all_chars(InputListe,[],Ergliste),
    reverse(Ergliste,Outputliste).

agg_label_all_chars([],Agg,Agg).

agg_label_all_chars([H|T],Agg,Outputliste):-
    char_type_char(H,Type,TChar),
    label_char(TChar,Type,ListElem),
    agg_label_all_chars(T,[ListElem|Agg],Outputliste).

label_char(H,letter,l(H)):-!.
label_char(H,whitespace,w(H)):-!.
label_char(H,special,s(H)):-!.    
label_char(H,digit,d(H)):-!.    

%%
%% Character classification
%%

% char_type_char(+Char,-Type,-TranslatedChar)
%   Classifies all characters as letter, digit, special, etc.,
%   and also translates each character into the character that
%   will represent it, converting upper to lower case.

char_type_char(Char,Type,Tr) :-
   char_table(Char,Type,Tr),
   !.

% Using downcase_atom saves having an enormous table
% and should handle all languages.
char_type_char(Char,letter,Char2) :-
   atom_chars(L2,[Char]),
   downcase_atom(L2,L3),
   atom_chars(L3,[Char2]).  


% Whitespace characters
char_table('\n', whitespace, '\n'  ).
char_table(' ',     whitespace,  ' ').     % blank
char_table('\t',    whitespace,  ' ').     % tab
char_table('\r',    whitespace,  ' ').     % return
%char_table('''',    whitespace, '''').     % apostroph wird als normaler Buchstabe behandelt.

% Donald removed the letter characters and replaced them by special characters.
% There are too many Unicode letters to put them all in a table.
% The third parameter may be useless, but maybe someone will want to convert
% some of the special characters.
% There may be other Unicode characters that need to be added.

char_table('~',     special,    '~' ).
char_table('`',     special,    '`' ).
char_table('!',     special,    '!' ).
char_table('@',     special,    '@' ).
char_table('#',     special,    '#' ).
char_table('$',     special,    '$' ).
char_table('\u0025',special,    '\u0025' ). %
char_table('^',     special,    '^' ).
char_table('&',     special,    '&' ).
char_table('*',     special,    '*' ).
char_table('(',     special,    '(' ).
char_table(')',     special,    ')' ).
char_table('_',     special,    '_' ).
char_table('-',     special,    '-' ).
char_table('+',     special,    '+' ).
char_table('=',     special,    '=' ).
char_table('{',     special,    '{' ).
char_table('[',     special,    '[' ).
char_table('}',     special,    '}' ).
char_table(']',     special,    ']' ).
char_table('|',     special,    '|' ).
char_table('\\',    special,    '\\' ).
char_table(':',     special,    ':' ).
char_table(';',     special,    ';' ).
char_table('"',     special,    '"' ).
char_table('<',     special,    '<' ).
char_table(',',     special,    ',' ).
char_table('>',     special,    '>' ).
char_table('.',     special,    '.' ).
char_table('?',     special,    '?' ).
char_table('/',     special,    '/' ).

% Digits
char_table('0',   digit,     '0' ).
char_table('1',   digit,     '1' ).
char_table('2',   digit,     '2' ).
char_table('3',   digit,     '3' ).
char_table('4',   digit,     '4' ).
char_table('5',   digit,     '5' ).
char_table('6',   digit,     '6' ).
char_table('7',   digit,     '7' ).
char_table('8',   digit,     '8' ).
char_table('9',   digit,     '9' ).

% Everything else is a letter character.
