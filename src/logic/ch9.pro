%% alles in Prolog ist ein Term / functor, sogar Listen:
%% .(a,[]) = [a]
%% Wenn etwas kein Term/functor ist, dann ist es meist syntactic sugar. 
%% Univ =..(complexTerm, X), X ist dann eine Liste des Terms. Das geht auch rückwarts - eine Liste wird dann zu einem komplexen Term!
%% Arithmetische Operatoren sind eigentlich auch Terme, aber dafür gibt es eine infix Notation.
%% ground terms sind Terme ohne Variablen
