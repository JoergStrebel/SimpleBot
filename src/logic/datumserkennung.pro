/*
Datumserkennung von J. Strebel, 2016

Funktionen des Moduls:
 - Erkennung von Datümern in deutscher Notation, z.B. Tag.Monat.Jahr
 - Es benötigt den Input des Tokenizers, da es sich die bereits vorhandenen Zahlen-Tokens zu Nutze macht.
 - Es liefert eine erweiterte Listenstruktur mit Datums-Tags zurück.

*/
:- module(datumserkennung, [erkenne_datümer/2]). % module muss als erste Regel stehen
:- use_module('tokenizer.pro').

/*
 Erkenne Datümer als Einstiegsprädikat ins Modul.
*/ 
erkenne_datümer(LInput,LOutput).

