% is/2 (Zahl is Ausdruck) prüft, ob eine Nummer mit einem Ausdruck matcht (Unification). Int und Float sind da unterschiedlich! Für Ausdruck kann keine Variable stehen, für Zahl schon. Ein Ausdruck ist erstmal nur ein Term, d.h. er hat keinen Wert. is forciert die Evaluierung.
% =:= prüft, ob die Werte zweier Ausdrücke gleich sind. Es kommt nur auf den Zahlenwert an.
% = ist nur Unification.

append([],L,L).
append([H|T],L2,[H|L3]) :- append(T,L2,L3).

palindrome(L) :- reverse(L,L). % liefert true zurück, wenn L ein Palindrom ist.

% Bauen einer flachen Liste
%flatten(LIST, FLAT) :- accflatten(LIST,[],FLAT).

%accflatten([],ACCL,ACCL).
%accflatten([H|_],ACCL,FLAT) :- is_list(H),accflatten(H,ACCL,FLAT).
%accflatten([H|T],[ACCL],FLAT) :- \+is_list(H),accflatten(T,ACCL,FLAT).

flatten([],[]). %1.Fall: leere Liste
flatten([[H|T1]|T2],FLAT) :- flatten(H,FLAT1),flatten(T1,FLAT2), flatten(T2,FLAT3),append([],FLAT1,FLATR1),append(FLATR1,FLAT2,FLATR2),append(FLATR2,FLAT3,FLAT) . %2. Fall: Head ist eine Liste
flatten([H|T],FLAT):- \+is_list(H), flatten(T,FLATT), append([H],FLATT,FLAT).  %3. Fall: Head ist ein Atom
flatten(AT,FLAT):- \+is_list(AT), append([AT],[],FLAT).

