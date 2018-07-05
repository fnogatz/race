:- module(race_ape, [
      check_consistency/3,
      ask_with_answers/3,
      prove_with_answers/3
   ]).
:- reexport('../race', except([
      check_consistency/2,
      prove_with_answers/3,
      ask_with_answers/3
   ])).

% library(ape) loads the `tokenizer` module, too
:- use_module(library(ape)).

check_consistency(Knowledge, Result, Variant) :-
   replace_variables(Knowledge, Variant),
   race:check_consistency(Variant, Result),
   Result \= error(_),
   !.

ask_with_answers(Knowledge, Question, Result) :-
   race:ask_with_answers(Knowledge, Question, Result).

prove_with_answers(Knowledge, Theorem, Result) :-
   race:prove_with_answers(Knowledge, Theorem, Result).

replace_variables(Text, Variant) :-
   tokenizer:tokenize(Text, Tokens),
   maplist(replace_possible_var, Tokens, TokenVariant),
   tokens_to_sentences:tokens_to_sentences(TokenVariant, SentencesTokenVariant),
   ace_niceace:tokens_to_sentences(SentencesTokenVariant, SentencesVariant),
   atomic_list_concat(SentencesVariant, ' ', Variant).

replace_possible_var(Token, Variant) :-
   atom_codes(Token, Codes),
   Codes = [C],  % one char only
   C >= 97,
   C =< 122,
   !,
   variant(C, Cn),
   atom_codes(Variant, [Cn]).

replace_possible_var(Token, Token).

% for "a" start with lowercase first
variant(97, Cn) :-
   !,
   ( Cn = 97 ; Cn = 65 ).
% for all other lowercase letters start with its capitalized version first
%   since it is a variable in most cases.
variant(C, Cn) :-
   ( Cn is C - 32
   ; Cn is C ).
