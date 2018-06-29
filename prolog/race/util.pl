:- module(util, [
      string_list_concat/2,
      join/3,
      pp/1
   ]).

string_list_concat([], "").
string_list_concat([X|Xs], S) :-
   string_list_concat(Xs, Ss),
   string_concat(X, Ss, S).

join(Glue, Ls, R) :-
   delete(Ls, "", Ls1),
   delete(Ls1, '', Ls2),
   join_(Glue, Ls2, R).

join_(_, [], "") :- !.
join_(_, [X], X) :- !.
join_(Glue, [X|Xs], R) :-
   join(Glue, Xs, R2),
   string_list_concat([X, Glue, R2], R).

% just for debugging purposes
pp(Term) :-
   print_term(Term, []),
   nl.
