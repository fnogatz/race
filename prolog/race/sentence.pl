:- module(sentence, [
      generate_answer/2
   ]).

:- use_module(library(race/util)).

generate_answer(proof(Entities), Answer) :-
   get_substitutions(Entities, Substitutions, Entities_Without_Substitutions),
   substitutions_fragment(Substitutions, Substitutions_Fragment),
   maplist(fragment, Entities_Without_Substitutions, EWS_Fragments),
   join(" and ", EWS_Fragments, EWS_Fragment),
   join(" and ", [ Substitutions_Fragment, EWS_Fragment], Answer).

generate_answer(not(Entities), Answer) :-
   maplist(fragment, Entities, Fragments),
   join(" ", Fragments, Answer).

get_substitutions(Entities, Substitutions, Entities_Without_Substitutions) :-
   maplist(get_only(substitution), Entities, Substitutions_R, Entities_Without_Substitutions_R),
   flatten(Substitutions_R, Substitutions),
   flatten(Entities_Without_Substitutions_R, Entities_Without_Substitutions).

get_only(What, Entity, Selected, NonSelected) :-
   ( Entity =.. [What|_] ->
     Selected = [ Entity ],
     NonSelected = []
   ; Selected = [],
     NonSelected = [ Entity ]).

substitutions_fragment([], '') :- !.
substitutions_fragment(Substitutions, JoinedFragments) :-
   maplist(fragment, Substitutions, Fragments),
   join(" and ", Fragments, JoinedFragments).

fragment(substitution(Left, Right), Fragment) :-
   !,
   string_list_concat([Left, " as ", Right], Fragment).
fragment(fact(F), Fragment) :-
   !,
   string_concat("the known fact that ", F, FragmentWithDot),
   string_concat(Fragment, ".", FragmentWithDot).
fragment(X, Fragment) :-
   % unknown case
   X =.. [_Type, Term|_],
   Fragment = Term.
