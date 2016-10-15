% Definite Clause Grammars

% Beispiel einer simplen Grammatik mit difference lists
% np nimmt den ganzen Satz in X und hinterlässt in Y den Rest ohne die Noun Phrase. Somit ist Y die difference list zu X. X-Y ist die Noun Phrase, mit Y als Rest wird weitergearbeitet.

%s(X,Z):- np(X,Y), vp(Y,Z).
%np(X,Z):- det(X,Y), n(Y,Z).
%vp(X,Z):-  v(X,Y), np(Y,Z).
%vp(X,Z):-  v(X,Z).
%det([the|W],W).
%det([a|W],W).
%n([woman|W],W).
%n([man|W],W).
%v([shoots|W],W).


% Beispiel mit DCG Notation
s --> np,vp.

np --> det,n.

vp --> v,np.
vp --> v.

det --> [the].
det --> [a].

n --> [woman].
n --> [man].

v --> [shoots].
