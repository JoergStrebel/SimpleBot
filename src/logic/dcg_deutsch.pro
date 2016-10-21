
% Kleine DCG für eine Teilmenge der deutschen Sprache
% start/2: erstes Argument ist der Satz, zweites Argument die leere Liste. Weitere optionale Argumente stehen vor diesen beiden.


%% Hilfsfunktionen

%% Definition of reverse([])
accRev([H|T],A,R):- accRev(T,[H|A],R).
accRev([],A,A).
rev(L,R):- accRev(L,[],R).

accstringlist_to_atomlist([HS|TS],AC,AL):- atom_string(AT,HS),accstringlist_to_atomlist(TS,[AT|AC],AL).
accstringlist_to_atomlist([],AC,AC).
stringlist_to_atomlist(SL,AL):- accstringlist_to_atomlist(SL,[],RAL),rev(RAL,AL).

%% Tokenizer für einen einfachen Satz
sentence_to_list(S,L):- string(S),split_string(S," ",".",STL),stringlist_to_atomlist(STL,L).


%% Grammatik
start --> question; statement.

question -->  qword, vp(NR) , np(NR).
qword --> [WORD],{lex(WORD,qw)}.

statement --> np(NR), vp(NR), np(NR).

np(NR) --> [WORD],{lex(WORD,propnoun)}.
np(NR) --> artnom(NR), nom(NR).

artnom(NR) --> [WORD],{lex(WORD,art(NR))}.

nom(NR) --> [WORD],{lex(WORD,object(NR))}.

vp(NR) --> [WORD],{lex(WORD,verb(NR))}.


%% Lexikon

lex('Jörg',propnoun).
lex('SimpleBot',propnoun).
lex('der',art(sing)).
lex('ein',art(sing)).
lex('Hund',object(sing)).
lex('Tisch',object(sing)).
lex('ist',verb(sing)).
lex('Was',qw).
lex('Wer',qw).

%% Simpler Term als Fakt in meiner DB:
malt('Jörg').

%% Frage: Wer malt?
%% Bedeutung nach dem Parsen: bedeutung(malt(X))
bedeutung(malt(X)).

%% Um jetzt eine Antwort auf die Frage zu erhalten ,muss Prolog in seiner DB nachschauen:
antwort(Z) :- bedeutung(X),X,arg(1,X,Z).
%% Allerdings ist diese Art der Abfrage nur für ganz einfache Bedeutungen möglich.
%% TODO: Wie können komplexere Bedeutungen in Prolog bewiesen werden? Wie kann ich nach der Bedeutungsermittlung die Bedeutung entweder zur Datenbank hinzufügen oder aus ihr abfragen?





