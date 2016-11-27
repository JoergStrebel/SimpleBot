
/*
Tokenizer von J. Strebel

Funktionen des Tokenizers:
 - Erkennung von  Interpunktionszeichen
 - Erkennung von Zahlen (sowohl Kardinalzahlen als auch Ordinalzahlen). Allerdings findet keine Erkennung von augeschriebenen Zahlen, z.B. vier, statt.
 - Erkennung von Sätzen und Aufspaltung des Inputs in mehrere Sätze, falls nötig
 - Ersetzung von Abkürzungen (z.B. Hr.) entweder durch die Langform oder durch Verkettung der dazugehörigen Tokens zu einem Wort.

D.h der Tokenizer soll einen String nehmen und dann einen oder mehrere Sätze zurückliefern.
Jeder Satz soll eine Liste mit Atomen sein in der Form w(<Atom>), n(<Zahl>). D.h. die Datenstruktur sieht dann so aus (am Beispiel des Satzes 'Ich gehe zu Fuß. Hilf mir!'):

?- tokenize_string("Ich gehe zu Fuß. Hilf mir!",T).
T = [satz([wort([i, c, h], groß), wort([g, e, h, e], klein), wort([z, u], klein), wort([f, u, ß], groß)]), ausrufesatz([wort([h, i, l, f], groß), wort([m, i, r], klein)])].

*/
:- module(tokenizer, [tokenize_string/2]). % module muss als erste Regel stehen

/* tokenize_string(+StrIn,-LSatz).*/
tokenize_string(StrIn,SenList) :- 
	string_chars(StrIn,Chars), 
	label_all_chars(Chars,LChars),
	collect_words(LChars,WList),
	collect_numbers(WList,WNrList),
	delete(WNrList,w(_),LSatz),
	findall(abkürz(A,B),abkürz(A,B),LAbk),
	ersetze_alle_abkürzungen(LAbk,LSatz,LExSatz),
	collect_sentences(LExSatz,SenList)
	.

/* Ersetze Abkürzungen. Wichtig für die Filterung von Punkten vor der Satzerkennung 
   ersetze_alle_abkürzungen(+Liste der Abkürzungen, +Wortliste,-expandierte Wortliste)
   ersetze_abkürzung(+Abkürzung, +Wortliste,-expandierte Wortliste)
   Vorgehen: nimm alle Abkürzungen und suche&ersetze sie im Satz
*/
ersetze_alle_abkürzungen([H|LAbk],LSatz,LExSatz):-
    ersetze_abkürzung(H,LSatz,LTmpSatz),
    ersetze_alle_abkürzungen(LAbk,LTmpSatz,LExSatz).

/* Ende der Rekursion, wenn alle Abkürzungen verarbeitet wurden*/
ersetze_alle_abkürzungen([],LSatz,LSatz):-!.

ersetze_abkürzung(abkürz(X,Y),LWorte,LExWorte):-
    replace_in_list(LWorte,X,Y,LExWorte),
    !.

/* Abkürzung nicht gefunden, gebe die Worte so zurück*/
ersetze_abkürzung(abkürz(_,_),LWorte,LWorte).


/* collect_sentences(+Liste von Wörtern und Satzzeichen, -Liste von Sätzen)
   Die Annahme ist, dass Punkte nur das Satzende markieren. Jede sonstige Verwendung von Punkten z.B. in Abkürzungen oder Datümern, muss vorher ersetzt werden.
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



/* 
 collect_numbers(+List of Tokens, -List with numbers)
 Baut einzelne Ziffern zu einer Zahl zusammen. Es gibt zwei Arten: Anzahl und Reihenfolge. Anzahl kann eine Ganzzahl oder eine Zahl mit Nachkommaanteil sein.
 Zahlenformat Anzahl: nnn,nn
 Zahlenformat Reihenfolge nn.
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

% Default: kein Beginn einer Zahl - kopiere das Token in den Output
agg_find_all_numbers([H|T],Agg,Outputliste):-
    agg_find_all_numbers(T,[H|Agg],Outputliste).


/* Prädikat vervollständigt eine Zahl; LInp startet mit einer Ziffer 
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


/* 
 labele alle Chars in der Liste 
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
Die Groß/Kleinschreibung wird bei der Ersetzung der Abkürzung übernommen.

*/
abkürz([wort([h, r], GK),s('.')],[wort([h,e,r,r], GK)]).
abkürz([wort([f, r], GK),s('.')],[wort([f,r,a,u], GK)]).
abkürz([wort([f, r,e,u,n,d,l], GK),s('.')],[wort([f,r,e,u,n,d,l,i,c,h|_], GK)]). %% Nur Wortstamm, Variable für Endung
abkürz([wort([d, r], GK),s('.')],[wort([d,o,k,t,o,r], GK)]).
abkürz([wort([p, r,o,f], GK),s('.')],[wort([p,r,o,f,e,s,s,o,r], GK)]).
abkürz([wort([s, t,r], GK),s('.')],[wort([s,t,r,a,ß,e], GK)]).
abkürz([wort([s, t,r], GK),s('.')],[wort([s,t,r,a,ß,e], GK)]).
abkürz([wort([j,a,n], GK),s('.')],[wort([j,a,n,u,a,r], GK)]).
abkürz([wort([f,e,b], GK),s('.')],[wort([f,e,b,r,u,a,r], GK)]).
abkürz([wort([m,r,z], GK),s('.')],[wort([m,ä,r,z], GK)]).
abkürz([wort([a,p,r], GK),s('.')],[wort([a,p,r,i,l], GK)]).
abkürz([wort([j,u,n], GK),s('.')],[wort([j,u,n,i], GK)]).
abkürz([wort([j,u,l], GK),s('.')],[wort([j,u,l,i], GK)]).
abkürz([wort([a,u,g], GK),s('.')],[wort([a,u,g,u,s,t], GK)]).
abkürz([wort([s,e,p], GK),s('.')],[wort([s,e,p,t,e,m,b,e,r], GK)]).
abkürz([wort([o,k,t], GK),s('.')],[wort([o,k,t,o,b,e,r], GK)]).
abkürz([wort([n,o,v], GK),s('.')],[wort([n,o,v,e,m,b,e,r], GK)]).
abkürz([wort([d,e,z], GK),s('.')],[wort([d,e,z,e,m,b,e,r], GK)]).
abkürz([wort([u,s,w], GK),s('.')],[wort([u,s,w,.], GK)]).
abkürz([wort([e,t,c], GK),s('.')],[wort([e,t,c,.], GK)]).
abkürz([wort([v], GK0),s('.'),wort([a], _),s('.')],[wort([v,.,a,.], GK0)]).
abkürz([wort([g,g,f], GK),s('.')],[wort([g,e,g,e,b,e,n,e,n,f,a,l,l,s], GK)]).
abkürz([s('.'),s('.'),s('.')],[s('.')]).
abkürz([wort([z], GK0),s('.'),wort([b], _),s('.')],[wort([z,.,b,.], GK0)]).
abkürz([zahl(Z),s('.')],[zahlrf(Z)]). %%Regel für die Erkennung von Ordinalzahlen


/* Hilfsfunktionen für Listen */

/* 
  replace_in_list(+Inputliste,+Suchliste,+Ersetzliste,-Ergebnisliste)
  Ersetzt eine Teilliste durch eine andere Teilliste
  Falls die Suchliste nicht gefunden wird, ist die Ergebnisliste gleich der Inputliste, ohne dass ein Fehler geworfen wird.
*/
replace_in_list(Inputliste,Suchliste,Ersetzliste,Ergebnisliste):-
    agg_replace_in_list(Inputliste,[],Suchliste,Ersetzliste,LTmpErg),
    reverse(LTmpErg,Ergebnisliste).

/* die Inputliste ist leer. Rekursionsende */
agg_replace_in_list([],Agg,_,_,Agg):-!.
    
/* Gutfall: die Suchliste wird gefunden. Variablen, die gleichzeitig in der Such- als auch der Ersetzliste stehen, werden erneuert, um unabhängige Mehrfachmatches zu ermöglichen. copy_term/2 muss am Anfang kommen, damit die noch uninstanziierten Variablen kopiert werden. Nach append/3 sind alle Variablen instanziiert.*/
agg_replace_in_list(Inputliste,Agg,Suchliste,Ersetzliste,Ergebnisliste):-
    copy_term(ttmp(Suchliste,Ersetzliste),ttmp(Suchliste1,Ersetzliste1)),
    append(Suchliste,Restliste,Inputliste),
    reverse(Ersetzliste,RErsetzliste),
    append(RErsetzliste,Agg,ZE),
    agg_replace_in_list(Restliste,ZE,Suchliste1,Ersetzliste1,Ergebnisliste),
    !.

/* Default: Die Suchliste wird noch nicht gefunden */
agg_replace_in_list([HIL|TIL],Agg,Suchliste,Ersetzliste,Ergebnisliste):-
    agg_replace_in_list(TIL,[HIL|Agg],Suchliste,Ersetzliste,Ergebnisliste).

