% http://lpn.swi-prolog.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse16
% Exercise 4.5

tran(eins,one).
tran(zwei,two).
tran(drei,three).
tran(vier,four).
tran(fuenf,five).
tran(sechs,six).
tran(sieben,seven).
tran(acht,eight).
tran(neun,nine).

% Zwei leere Listen sind immer äquivalent.
listtran([],[]).
% Ich will zeigen dass L1 und L2 elementweise äquivalente Deutsch-Englisch Übersetzungslisten sind.
listtran([HD|TD],[HE|TE]):- tran(HD,HE),listtran(TD,TE).
 
