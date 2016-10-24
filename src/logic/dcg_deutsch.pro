:- dynamic concept/2.

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

start(St,M) :- sentence_to_list(St,Li),sentence(M,Li,[]).
%TODO: evaluate/1 erstellen

%% Grammatik
% Kleine DCG für eine Teilmenge der deutschen Sprache
% start/2: erstes Argument ist der Satz, zweites Argument die leere Liste. Weitere optionale Argumente stehen vor diesen beiden. M als Argument ist die Bedeutungsrepräsentation des Satzes. 

sentence(M) --> question(M),!. 
sentence(M) --> statement(M).

question(allgetproperty(Wd,Prop,Z)) -->  qword, vp(Prop,NR) , np(Wd,NR).
qword --> [WORD],{lex(WORD,qw)}.

statement(addproperty(Wd, Prop, Val)) --> np(Wd,NR), vp(Prop,NR), np(Val,NR).

np(Wd,_) --> [WORD],{lex(WORD,propnoun),Wd=WORD}.
np(Wd,NR) --> artnom(NR), nom(Wd,NR).

artnom(NR) --> [WORD],{lex(WORD,art(NR))}.

nom(Wd,NR) --> [WORD],{lex(WORD,object(NR)),Wd=WORD}.

vp(Prop,NR) --> [WORD],{lex(WORD,verb(NR),mean(Prop))}.


%% Lexikon

lex('Jörg',propnoun).
lex('SimpleBot',propnoun).
lex('Mensch',object(sing)).
lex('der',art(sing)).
lex('ein',art(sing)).
lex('Hund',object(sing)).
lex('Tisch',object(sing)).
lex('ist',verb(sing),mean(is_a)).
lex('Was',qw).
lex('Wer',qw).
lex('Tier',object(sing)).

%% Simpler Term als Fakt in meiner DB:
%% malt('Jörg').

%% Frage: Wer malt?
%% Bedeutung nach dem Parsen: bedeutung(malt(X))
%% bedeutung(malt(X)).

%% Um jetzt eine Antwort auf die Frage zu erhalten ,muss Prolog in seiner DB nachschauen:
%% antwort(Z) :- bedeutung(X),X,arg(1,X,Z).
%% Allerdings ist diese Art der Abfrage nur für ganz einfache Bedeutungen möglich.


%% Datenbank
%% Einträge für Verb "sein" im Sinne einer Subsumption einer Klasse unter eine andere und im Sinne einer Instanziierung

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



