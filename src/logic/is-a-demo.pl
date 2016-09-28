% Prolog-Test

% Fakten

wort(hund, [is_a = tier]).
wort(tisch, [is_a = möbel]).
wort(baum, [is_a = pflanze]).
wort(fisch, [is_a = tier]).

wort(tier, [is_a = objekt]).
wort(möbel, [is_a = objekt]).
wort(pflanze, [is_a = objekt]).
wort(objekt, [is_a = alles]).

wort(simplebot, [instance_of = chatbot]).
wort(nutzer, [instance_of = mensch]).

transitiv(is_a).

% Regeln 
% abgeleitet aus http://www.trilug.org/pipermail/dev/2003-September/000368.html
eigenschaft(Wort, Prop, Val) :-
    wort(Wort, PropList),
    member(Prop = Val, PropList).

eigenschaft(Wort, Prop, Val) :-
    transitiv(Prop),
    wort(Wort, PropList),
    member(Prop = Wort2, PropList),
    eigenschaft(Wort2,Prop,Val).





