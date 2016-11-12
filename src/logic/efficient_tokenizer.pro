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


%%
%% User-callable routines
%%
/*
% tokens_words(+Tokens,-Words)
%  From the output of the other routines, extracts just
%  the word tokens and converts them to atoms.

tokens_words([],[]).

tokens_words([w(Chars)|Tokens],[Atom|Atoms]) :-
   !,
   atom_chars(Atom,Chars),
   tokens_words(Tokens,Atoms).

tokens_words([_|Tokens],Atoms) :-
   % skip non-word tokens
   tokens_words(Tokens,Atoms).



% tokenize_file(+Filename,-Tokens)
%  Reads an entire file and tokenizes it.

tokenize_file(Filename,Tokens) :-
   open(Filename,read,Stream),
   tokenize_stream(Stream,Tokens),
   close(Stream).

% tokenize_stream(+Stream,-Tokens)
%  Reads an entire stream and tokenizes it.

tokenize_stream(Stream,[]) :-
   at_end_of_stream(Stream),
   !.

tokenize_stream(Stream,Tokens) :-
   tokenize_line_dl(Stream,Tokens/Tail),
   tokenize_stream(Stream,Tail).
*/   

% tokenize_line_dl(+Stream,-Tokens/Tail)
%  Like tokenize_line, but uses a difference list.
%  This makes it easier to append the results of successive calls.

/* tokenize_string(+StrIn,-LSatz).*/
tokenize_string(StrIn,Tokens) :- 
	string_chars(StrIn,Chars), 
	tokenize_line_dl(Chars,Tokens/_).

at_end_of_string([]).

tokenize_line_dl(CharList,_/[]) :-
   at_end_of_string(CharList),                        
   !.

tokenize_line_dl(CharList,Dlist) :-
   get_char_and_type(CharList,Char,Type,ChLRest), %%hole das erste Zeichen
   tokenize_line_x(Type,Char,ChLRest,Dlist).


% tokenize_line_x(+Type,+Char,+Stream,-Tokens/Tail)
%  Tokenizes (the rest of) a line of input.
%  Type and Char describe the character that has been read ahead.

% Das stellt die Schleife über die Input Char List dar.
tokenize_line_x(eol,_,_,_/[]) :-               % end of line mark; terminate
   !.

tokenize_line_x(whitespace,_,Rest,Dlist) :-       % whitespace, skip it
   !,
   tokenize_line_dl(Rest,Dlist).


% Word tokens and number tokens have to be completed,
% maintaining 1 character of read-ahead as this is done.
% NewChar and NewType are the character read ahead
% after completing the token.

tokenize_line_x(letter,Char,CharList,[w(T)|Tail]/Tail) :-
   !,
   tokenize_letters(letter,Char,CharList,T,NewType,NewChar,CL2),
   tokenize_line_x(NewType,NewChar,CL2,Tokens/Tail).

tokenize_line_x(digit,Char,Stream,[n(T)|Tail]/Tail) :-
   !,
   tokenize_digits(digit,Char,Stream,T,NewType,NewChar,Stream2),
   tokenize_line_x(NewType,NewChar,Stream2,Tokens/Tail).

/*
% A period is handled like a digit if it is followed by a digit.
% This handles numbers that are written with the decimal point first.

tokenize_line_x(_, '.', Stream,Dlist) :-
   peek_char(Stream,P),
   char_type_char(P,digit,_),
   !,
   % Start over, classifying '.' as a digit
   tokenize_line_x(digit, '.', Stream,Dlist).

% Donald: A comma is also handled like a digit if it is followed by a digit.
% For those locales where a comma is used as a decimal separator.
% Users may want to allow a decimal separator (say ',') but remove 
% the (ten)thousands separator (say '.'). Further coding is required for this.

tokenize_line_x(_, ',', Stream,Dlist) :-
   peek_char(Stream,P),
   char_type_char(P,digit,_),
   !,
   % Start over, classifying ',' as a digit
   tokenize_line_x(digit, ',', Stream,Dlist).
*/

% Special characters and unidentified characters are easy:
% they stand by themselves, and the next token begins with
% the very next character.

tokenize_line_x(special,Char,Stream,[s(Char)|Tail]/Tail) :-   % special char
   !,
   tokenize_line_dl(Stream,Tokens/Tail).

tokenize_line_x(_,Char,Stream,[other(Char)|Tail]/Tail) :-     % unidentified char
   !,
   tokenize_line_dl(Stream,Tokens/Tail).



% tokenize_letters(+Type,+Char,+Stream,-Token,-NewChar,-NewType,-Stream2)
%   Completes a word token beginning with Char, which has
%   been read ahead and identified as type Type.
%   When the process ends, NewChar and NewType are the
%   character that was read ahead after the token.
%  Token ist eine Liste von aufeinanderfolgenden Buchstaben
% Stream2 als neue verkürzte Inputliste

tokenize_letters(letter,Char,Stream,[Char|Rest],NewType,NewChar,Stream2) :-
   % It's a letter, so process it, read another character ahead, and recurse.
   !,
   get_char_and_type(Stream,Char2,Type2,Stream2),
   tokenize_letters(Type2,Char2,Stream2,Rest,NewType,NewChar,_).

tokenize_letters(Type,Char,CL1,[],Type,Char,CL1).
   % It's not a letter, so don't process it; pass it to the calling procedure.


% tokenize_digits(+Type,+Char,+Stream,-Token,-NewChar,-NewType, -Stream2)
%   Like tokenize_letters, but completes a number token instead.
%   Additional subtleties for commas and decimal points.

tokenize_digits(digit,Char,Stream,[Char|Rest],NewType,NewChar,Stream2) :-
   % It's a digit, so process it, read another character ahead, and recurse.
   !,
   get_char_and_type(Stream,Char2,Type2,Stream2),
   tokenize_digits(Type2,Char2,Stream2,Rest,NewType,NewChar,_).

/*tokenize_digits(_, '.', Stream,['.'|Rest],NewType,NewChar) :-
   peek_char(Stream,P),
   char_type_char(P,digit,Char2),
   !,
   % It's a period followed by a digit, so include it and continue.
   get_char(Stream,_),
   tokenize_digits(digit,Char2,Stream,Rest,NewType,NewChar).

% Donald added this clause for commas in numbers:

tokenize_digits(_, ',', Stream,[','|Rest],NewType,NewChar) :-
   peek_char(Stream,P),
   char_type_char(P,digit,Char2),
   !,
   % It's a comma followed by a digit, so include it and continue.
   get_char(Stream,_),
   tokenize_digits(digit,Char2,Stream,Rest,NewType,NewChar).
*/

tokenize_digits(Type,Char,CL1,[],Type,Char,CL1).
   % It's not any of those, so don't process it;
   % pass it to the calling procedure.


%%
%% Character classification
%%

peek_char_string([H|_],H).

% get_char_and_type(+InputStringList,-Char,-Type,-ResStrList)
%  Reads a character, determines its type, and translates
%  it as specified in char_type_char.

get_char_and_type([H|T],Char,Type,T) :-
   char_type_char(H,Type,Char).

get_char_and_type([],end_of_file,eol,[]).


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

% End of line marks
char_table(end_of_file, eol, end_of_file).
char_table('\n',        eol, '\n'       ).

% Whitespace characters
char_table(' ',     whitespace,  ' ').     % blank
char_table('\t',    whitespace,  ' ').     % tab
char_table('\r',    whitespace,  ' ').     % return
char_table('''',    whitespace, '''').     % apostrophe does not translate to blank

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