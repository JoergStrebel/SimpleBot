
/*
Tokenizer von J. Strebel

Funktionen des Tokenizers:
 - Erkennung von  Interpunktionszeichen
 - Erkennung von Zahlen
 - Aufspaltung des Inputs in mehrere Sätze, falls nötig
 - Ersetzung von Abkürzungen (z.B. Hr.)
 - Erkennung von Datümern (z.B. 13.12.2016)

D.h der Tokenizer soll einen String nehmen und dann einen oder mehrere Sätze zurückliefern.
Jeder Satz soll eine Liste mit Atomen sein in der Form w(<Atom>), n(<Zahl>). D.h. die Datenstruktur sieht dann so aus (am Beispiel des Satzes 'Ich gehe zu Fuß. Hilf mir!'):

?- tokenize_string("Ich gehe zu Fuß. Hilf mir!",T).
T = [satz([wort([i, c, h], groß), wort([g, e, h, e], klein), wort([z, u], klein), wort([f, u, ß], groß)]), ausrufesatz([wort([h, i, l, f], groß), wort([m, i, r], klein)])].

*/


/* tokenize_string(+StrIn,-LSatz).*/
tokenize_string(StrIn,SenList) :- 
	string_chars(StrIn,Chars), 
	label_all_chars(Chars,LChars),
	collect_words(LChars,WList),
	collect_numbers(WList,WNrList),
	delete(WNrList,w(_),LSatz),
	ersetze_abkürzungen(LSatz,LExSatz),
	collect_sentences(LExSatz,SenList)
	.

/* Ersetze Abkürzungen. Wichtig für die Filterung von Punkten vor der Satzerkennung 
   ersetze_abkürzungen(+Wortliste,-expandierte Wortliste)
   Vorgehen: nimm alle Abkürzungen und suche&ersetze sie im Satz
   TODO: prüfe alle Abkürzungen mittels findall(abkürz(A,B),abkürz(A,B),D)
*/

ersetze_abkürzungen(LWorte,LExWorte):-
    abkürz(X,Y),
    replace_in_list(LWorte,X,Y,LExWorte),
    !.

/* Abkürzung nicht gefunden, gebe die Worte so zurück*/
ersetze_abkürzungen(LWorte,LWorte).


/* collect_sentences(+Liste von Wörtern und Satzzeichen, -Liste von Sätzen)
*/
collect_sentences(LWords, WList):-
  agg_find_all_sentences(LWords,[],Erlist),
  reverse(Erlist,WList).

agg_find_all_sentences([],Agg,Agg):-!.

agg_find_all_sentences([H|T],Agg,Outputliste):-
    find_sentence([H|T],[],Result,Rest,Typ),
    reverse(Result,RevResult),
    HS=..[Typ|[RevResult]],
    agg_find_all_sentences(Rest,[HS|Agg],Outputliste).

/* vervollständige einen Satz; LInp fängt mit einem Nicht-Whitespace an
find_sentence(+LInp,-LWord, -Result, -Rest)
*/
%Satzzeichen erkannt, stoppe Rekursion
find_sentence([s('.')|LInp], LWord, LWord,LInp,'satz'):-!.
find_sentence([s('!')|LInp], LWord, LWord,LInp,'ausrufesatz'):-!.
find_sentence([s(';')|LInp], LWord, LWord,LInp,'satz'):-!.
find_sentence([s('?')|LInp], LWord, LWord,LInp,'fragesatz'):-!.

% Default: kopiere jedes Listenelement in den Output
find_sentence([H|LInp], Agg, Result, Rest,Typ):-
    find_sentence(LInp,[H|Agg], Result, Rest,Typ).

find_sentence([], LWord, LWord,[],'satz').


/* collect_words(+List of Tokens, -List with words)
Mache aus Einzeltokens Wörter
transformiert alle Wörter zur Kleinschreibung und annotiert das Wort entsprechend.
wort([..],groß)
*/
collect_words(LTokens, WList):-
  agg_find_all_words(LTokens,[],Erlist),
  reverse(Erlist,WList).

agg_find_all_words([],Agg,Agg):-!.

agg_find_all_words([w(_),l(H2)|T],Agg,Outputliste):-
    find_word([l(H2)|T],[],Result,Rest),
    reverse(Result,RevResult),
    schreibung(RevResult,KlRevResult,GKSch),
    agg_find_all_words(Rest,[wort(KlRevResult,GKSch)|Agg],Outputliste),
    !.

agg_find_all_words([l(H1)|T],Agg,Outputliste):-
    find_word([l(H1)|T],[],Result,Rest),
    reverse(Result,RevResult),
    schreibung(RevResult,KlRevResult,GKSch),
    agg_find_all_words(Rest,[wort(KlRevResult,GKSch)|Agg],Outputliste),
    !.

% Default: not a word start - just copy the token to the output
agg_find_all_words([H|T],Agg,Outputliste):-
    agg_find_all_words(T,[H|Agg],Outputliste).

/* schreibung(+Liste mit Atomen,-Liste kleingeschriebenen Atomen, -Code für Schreibung)
*/
schreibung([HA|LAtoms],LklAtoms,GKSch):-
    downcase_atom(HA,L3),
    L3=HA,
    LklAtoms=[HA|LAtoms],
    GKSch='klein',!.

/* HA muss groß sein*/
schreibung([HA|LAtoms],LklAtoms,GKSch):-
    downcase_atom(HA,L3),
    LklAtoms=[L3|LAtoms],
    GKSch='groß'.


/* completes a word; LInp starts with a word 
find_word(+LInp,-LWord, -Result, -Rest)
*/
find_word([l(T)|LInp], Agg, Result, Rest):-
    find_word(LInp,[T|Agg], Result, Rest),
    !.

find_word([], LWord, LWord,[]):-!.

% Default: not a letter, stop recursion
find_word([T|LInp], LWord, LWord,[T|LInp]).



/* collect_numbers(+List of Tokens, -List with numbers)
*/
collect_numbers(LTokens,NRList):-
  agg_find_all_numbers(LTokens,[],Erlist),
  reverse(Erlist,NRList).

agg_find_all_numbers([],Agg,Agg):-!.

agg_find_all_numbers([w(_),d(H2)|T],Agg,Outputliste):-
    find_number([d(H2)|T],[],Result,Rest),
    reverse(Result,RevResult),
    agg_find_all_numbers(Rest,[zahl(RevResult)|Agg],Outputliste),
    !.

agg_find_all_numbers([d(H1)|T],Agg,Outputliste):-
    find_number([d(H1)|T],[],Result,Rest),
    reverse(Result,RevResult),
    agg_find_all_numbers(Rest,[zahl(RevResult)|Agg],Outputliste),
    !.

% Default: not a number start - just copy the token to the output
agg_find_all_numbers([H|T],Agg,Outputliste):-
    agg_find_all_numbers(T,[H|Agg],Outputliste).


/* completes a number; LInp starts with a digit 
find_number(+LInp,-LWord, -Result, -Rest)
*/
% Zahlen mit Nachkommaanteil
find_number([d(H1),s(','),d(H2)|LInp], Agg, Result, Rest):-
    find_number(LInp,[H2,',',H1|Agg], Result, Rest),
    !.

% Ganzzahlen
find_number([d(T)|LInp], Agg, Result, Rest):-
    find_number(LInp,[T|Agg], Result, Rest),
    !.

find_number([], LWord, LWord,[]):-!.

% Default: not a number, stop recursion
find_number([T|LInp], LWord, LWord,[T|LInp]).


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

% Efficient Tokenizer starting here
% et.pl - M. Covington      2003 February 12

% etu.pl - Modified for Unicode - Donald Rogers     2006 July 17
%          email: dero9753@ihug.co.nz
%          Modified to cope with comma in numbers   2006 July 20

% ET the Efficient Tokenizer
% see http://www.covingtoninnovations.com/mc/ProNTo/index.html

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

% Default: wenn es nichts anderes ist, ist es ein Buchstabe
char_type_char(Char,letter,Char).


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
char_table('!',     special,    '!' ).
char_table('~',     special,    '~' ).
char_table('`',     special,    '`' ).
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


/* Liste mit deutschen Abkürzungen 
abkürz(+Atom,-Atom).
Das Input-Atom ist immer klein geschrieben, das Output Atom immer groß.
*/
abkürz([wort([h, r], GK),s('.')],[wort([h,e,r,r], GK)]).
abkürz([wort([f, r], GK),s('.')],[wort([f,r,a,u], GK)]).
/*
abkürz('fr.','Frau').
abkürz('freundl.','freundlichen').
abkürz('dr.','Doktor').
abkürz('prof.','Professor').
abkürz('str.','Straße').
abkürz('jan.','Januar').
abkürz('feb.','Februar').
abkürz('mrz.','März').
abkürz('apr.','April').
abkürz('jun.','Juni').
abkürz('jul.','Juli').
abkürz('aug.','August').
abkürz('sep.','September').
abkürz('okt.','Oktober').
abkürz('nov.','November').
abkürz('dez.','Dezember').
abkürz('usw.','und so weiter').
abkürz('etc.','et cetera').
abkürz('v.a.','vor allem').
abkürz('ggf.','gegebenenfalls').
*/

/* Hilfsfunktionen für Listen */

/* replace_in_list(+Inputliste,+Suchliste,+Ersetzliste,-Ergebnisliste)
Ersetzt eine Teilliste durch eine andere Teilliste
*/

replace_in_list(Inputliste,Suchliste,Ersetzliste,Ergebnisliste):-
    agg_replace_in_list(Inputliste,[],Suchliste,Ersetzliste,Ergebnisliste).

/* Schlechtfall: die Inputliste ist leer. Rekursionsende */
agg_replace_in_list([],_,_,_,[]):-!,fail.
    
/* Gutfall: die Suchliste wird gefunden. Rekursionsende */
agg_replace_in_list(Inputliste,Agg,Suchliste,Ersetzliste,Ergebnisliste):-
    append(Suchliste,Restliste,Inputliste),
    reverse(Agg,RAgg),
    append(RAgg,Ersetzliste,ZE),
    append(ZE,Restliste,Ergebnisliste), !.

/* Die Suchliste wird noch nicht gefunden */
agg_replace_in_list([HIL|TIL],Agg,Suchliste,Ersetzliste,Ergebnisliste):-
    agg_replace_in_list(TIL,[HIL|Agg],Suchliste,Ersetzliste,Ergebnisliste).

