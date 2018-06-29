:- module(axiom, [
      axiom_to_entity/2
   ]).

axiom_to_entity(Axiom, substitution(Left, Right)) :-
   string_concat('Substitution: ', Eq, Axiom),
   !,
   split_string(Eq, "=", " ", [Left, Right]).

axiom_to_entity(Axiom, name(Name)) :-
   string_concat('proper name: ', Name, Axiom),
   !.

axiom_to_entity(Axiom, verb(VerbWithS)) :-
   string_concat('transitive verb: ', Verb, Axiom),
   !,
   string_concat(Verb, "s", VerbWithS).

axiom_to_entity(Axiom, fact(F)) :-
   split_string(Axiom, ":", " ", [FactNumber, F]),
   number_string(_, FactNumber),
   !.

axiom_to_entity(Axiom, countable_noun(E)) :-
   string_concat("countable common noun: ", E, Axiom),
   !.

axiom_to_entity(Axiom, mass_common_noun(E)) :-
   string_concat("mass common noun: ", E, Axiom),
   !.

axiom_to_entity(Axiom, axiom(Axiom)) :- !.
