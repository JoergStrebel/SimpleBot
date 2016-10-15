% Kleine DCG für eine Teilmenge der deutschen Sprache
% start/2: erstes Argument ist der Satz, zweites Argument die leere Liste

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
lex( 'der',art(sing)).
lex( 'ein',art(sing)).
lex( 'Hund',object(sing)).
lex( 'Tisch',object(sing)).
lex( 'ist',verb(sing)).
lex( 'Was',qw).
lex( 'Wer',qw).




