% http://lpn.swi-prolog.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse1
% Coding convention: Atoms immer in Anführungszeichen, Variablen immer groß geschrieben
% listing(). um Code in der Kommandozeile abzufragen.
% trace und notrace für die Anzeige der Ableitung.
% Faustregel: links-rekursive Regeln können zu nicht-terminierenden Programmen führen. Daher auf die Reihenfolge der zu beweisenden Ziele im Körper der Regel achten.
% Unification einer Liste erfolgt elementweise, d.h. eine Variable matcht nur ein Listenelement, nicht einen beliebigen Teil der Liste. Den Rest der Liste bekommt man mit |.


'mia'. %Atoms
'jody'.
'yolanda'.
'vincent'.
'marsellus'.
'happy'. %Stelligkeit 0
'Werner'.
'Hans'.

woman('mia'). %predicate
woman('jody'). %predicate
woman('yolanda').%predicate
% playsAirGuitar(jody).
 
happy('yolanda').%predicate, Stelligkeit 1
listens2Music('mia').%predicate
listens2Music('yolanda'):- happy('yolanda').
playsAirGuitar('mia'):- listens2Music('mia').
playsAirGuitar('yolanda'):- listens2Music('yolanda'), 'mia'. %Komma ist "und", Strichpunkt ist "oder"
playsAirGuitar('jody'):- listens2Music('jody').

gooddancer('vincent').
loves('vincent','mia').
loves('marsellus','mia'). %parameterreihenfolge ist von Bedeutung
loves('mia',X):- gooddancer(X).

jealous(X,Y):- loves(X,Z), loves(Y,Z). %X ist eifersüchtig auf Y, Regel aber falsch, da sie auch gilt für X=Y.

vertical(line(point(X,Y),point(X,Z))).
horizontal(line(point(X,Y),point(Z,Y))).

numeral(0).
numeral(succ(X)) :- numeral(X).

a2b([],[]).
a2b([a|Ta],[b|Tb]) :- a2b(Ta,Tb). % Variablen matchen die leere Liste, oder jede andere Liste in Gänze.
