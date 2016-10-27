:- dynamic concept/2.
:- module(database, [start/2]).

%% Hilfsfunktionen

%% Definition of reverse([])
accRev([H|T],A,R):- accRev(T,[H|A],R).
accRev([],A,A).
rev(L,R):- accRev(L,[],R).

accstringlist_to_atomlist([HS|TS],AC,AL):- atom_string(AT,HS),accstringlist_to_atomlist(TS,[AT|AC],AL).
accstringlist_to_atomlist([],AC,AC).
stringlist_to_atomlist(SL,AL):- accstringlist_to_atomlist(SL,[],RAL),rev(RAL,AL).

%% Tokenizer für einen einfachen Satz
sentence_to_list(S,L):- var(S),!,L=[].
sentence_to_list(S,L):- string(S),split_string(S," ",".",STL),stringlist_to_atomlist(STL,L).

list_to_sentence(L,S):- var(L),!,S="".
list_to_sentence(L,S):- is_list(L),atomics_to_string(L,' ',S).

/*
Start des Programms hier
TODO: evil hack ausbauen
*/
start(ISt,ASt) :- sentence_to_list(ISt,Li),sentence(M,Li,[]),evalquest(M,ASt).
evalquest(M, ASt) :- call(M),allgetproperty(A,B,[C|_])=M,sentence(addproperty(A,B,C),X,[]),list_to_sentence(X,ASt).

%% Grammatik
% Kleine DCG für eine Teilmenge der deutschen Sprache
% start/2: erstes Argument ist der Satz, zweites Argument die leere Liste. Weitere optionale Argumente stehen vor diesen beiden. M als Argument ist die Bedeutungsrepräsentation des Satzes. 

sentence(M) --> question(M),!. 
sentence(M) --> statement(M).

question(allgetproperty(Wd,Prop,_)) -->  qword, vp(Prop,NR) , np(Wd,NR,_).

qword --> [WORD],{lex(WORD,qw)}.

statement(addproperty(Wd, Prop, Val)) --> np(Wd,NR,_), vp(Prop,NR), np(Val,_,_).

np(Wd,NR,GEN) --> [WORD],{lex(WORD,propnoun(NR,GEN)),Wd=WORD}.
np(Wd,NR,GEN) --> artnom(NR,GEN), nom(Wd,NR,GEN).

artnom(NR,GEN) --> [WORD],{lex(WORD,art(NR,GEN))}.

nom(Wd,NR,GEN) --> [WORD],{lex(WORD,object(NR,GEN)),Wd=WORD}.

vp(Prop,NR) --> [WORD],{lex(WORD,verb(NR),mean(Prop))}.


/* Lexikon
TODO: Mapping von Symbolen auf Konzepte 

*/

lex('Jörg',propnoun(sing,m)).
lex('SimpleBot',propnoun(sing,m)).
lex('Mensch',object(sing,m)).

lex('der',art(sing,m)).
lex('ein',art(sing,m)).
lex('das',art(sing,s)).
lex('ein',art(sing,s)).

lex('Hund',object(sing,m)).
lex('Tisch',object(sing,m)).
lex('Was',qw).
lex('Wer',qw).
lex('Tier',object(sing,s)).

lex('ist',verb(sing),mean(is_a)).


%% Datenbank
%% Einträge für Verb "sein" im Sinne einer Subsumption einer Klasse unter eine andere und im Sinne einer Instanziierung
% TODO: Umstellen der Konzepte auf Frames.

concept('Hund', [is_a = 'Tier']).
concept('Mensch', [is_a = objekt]).
concept('Tisch', [is_a = möbel]).
concept('Baum', [is_a = pflanze]).
concept('Fisch', [is_a = tier]).

concept('Tier', [is_a = objekt]).
concept(möbel, [is_a = objekt]).
concept(pflanze, [is_a = objekt]).
concept(objekt, [is_a = alles]).

concept(simplebot, [instance_of = chatbot]).
concept('Jörg', [instance_of = 'Mensch']).

transitiv(is_a).

% Datenbank-Schnittstelle
% Zwei Funktionen: getproperty und setproperty
% abgeleitet aus http://www.trilug.org/pipermail/dev/2003-September/000368.html


allgetproperty(Wort, Prop, Z) :- findall(Val,getproperty(Wort,Prop,Val),Z).

getproperty(Wort, Prop, Val) :-
    concept(Wort, PropList),
    member(Prop = Val, PropList).

getproperty(Wort, Prop, Val) :-
    transitiv(Prop),
    concept(Wort, PropList),
    member(Prop = Wort2, PropList),
    getproperty(Wort2,Prop,Val).

addproperty(Wort, Prop, Val) :- concept(Wort, PropList),retractall(concept(Wort,_)), assert(concept(Wort,[Prop=Val|PropList])).



