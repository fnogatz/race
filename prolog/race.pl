:- module(race, [
      check_consistency/1,
      check_consistency/2,
      ask/3,
      ask_for_answers/3,
      prove/3
   ]).

:- use_module(library(race/axiom)).
:- use_module(library(race/sentence)).
:- use_module(library(race/util)).

:- use_module(library(wsdl)).
:- use_module(library(xpath)).
:- use_module(library(soap)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_header)).

% :- debug(soap).

% :- multifile sgml_write:xmlns/2.
% sgml_write:xmlns(race, 'http://attempto.ifi.uzh.ch/race').

:- initialization wsdl_read(library(race/wsdl/'race.wsdl')).


check_consistency(Knowledge) :-
   check_consistency(Knowledge, []).

check_consistency(Knowledge, Result) :-
   Operation = ('http://attempto.ifi.uzh.ch/race':'RacePortType') /
               ('http://attempto.ifi.uzh.ch/race':'RunRace'),
   soap_call(Operation, [
      'Axioms'=Knowledge,
      'Mode'='check_consistency'
      %, 'Parameter'='raw'
   ], [ReplyDOM], [dom]),
   get_inconsistencies(ReplyDOM, Result).

ask(Knowledge, Question, Result) :-
   Operation = ('http://attempto.ifi.uzh.ch/race':'RacePortType') /
               ('http://attempto.ifi.uzh.ch/race':'RunRace'),
   soap_call(Operation, [
      'Axioms'=Knowledge,
      'Mode'='answer_query',
      'Theorems'=Question
      % , 'Parameter'='raw'
   ], ReplyDOM, [dom]),
   get_result(ReplyDOM, Result).

ask_for_answers(Knowledge, Question, Result) :-
   ask(Knowledge, Question, R),
   ( R = results(Proofs) ->
     maplist(generate_answer, Proofs, Answers),
     Result = results(Answers)
   ; R = not(WhyNot) ->
     generate_answer(not(WhyNot), Answer),
     Result = not(Answer)
   ; Result = R).

prove(Knowledge, Theorem, Result) :-
   Operation = ('http://attempto.ifi.uzh.ch/race':'RacePortType') /
               ('http://attempto.ifi.uzh.ch/race':'RunRace'),
   soap_call(Operation, [
      'Axioms'=Knowledge,
      'Mode'='prove',
      'Theorems'=Theorem
      % , 'Parameter'='raw'
   ], ReplyDOM, [dom]),
   get_result(ReplyDOM, Result).

get_inconsistencies(ReplyDOM, Result) :-
   findall(Axiom, xpath_select(ReplyDOM, 'Axiom', element(_, _, [Axiom])), Axioms),
   maplist(axiom_to_entity, Axioms, Result).

get_result(ReplyDOM, Result) :-
   findall(ProofDOM, xpath_select(ReplyDOM, 'Proof', ProofDOM), ProofDOMs),
   findall(WhyNotDOM, xpath_select(ReplyDOM, 'WhyNot', WhyNotDOM), WhyNotDOMs),
   ( ( ProofDOMs \= [], WhyNotDOMs = []) ->
     proof_list(ProofDOMs, Proof),
     Result = results(Proof)
   ; ( ProofDOMs = [], WhyNotDOMs \= []) ->
     why_not_list(WhyNotDOMs, WhyNot),
     Result = not(WhyNot)
   ; Result = error('Sorry, there are arguments for both sides.')).

proof_list(ProofDOMs, Result) :-
   maplist(proof, ProofDOMs, Result).

proof(ProofDOM, proof(Entities)) :-
   findall(Axiom, xpath_select(ProofDOM, 'Axiom', element(_, _, [Axiom])), Axioms),
   maplist(axiom_to_entity, Axioms, Entities).

why_not_list([WhyNotDOM], Entities) :-
   findall(Word, xpath_select(WhyNotDOM, 'Word', element(_, _, [Word])), Words),
   maplist(axiom_to_entity, Words, Entities).

xpath_select(DOM, Element, Result) :-
   xpath(DOM, //(_:Element), Result),
   Result \= element(_, _, []).

:- meta_predicate soap_call(:, +, -, +).
soap_call(Operation, Input, Reply, Options) :-
   Operation = M:_,
%  Version = soap12,    % TBD: How to sort this out nicely
   wsdl_function(Operation, Version, URL, Action,
            InputElements, OutputElements), !,
   debug(soap, '~w: URL=~q', [Version, URL]),
   soap:soap_action(Action, Version, SoapOptions),
   assertion(length(InputElements, 1)),
   assertion(length(InputElements, 1)),
   InputElements = [arg(_Name, element(InputElement))],
   xml_schema:xsd_create_element(InputElement, M:Input, InputContentDOM0),
   soap:dom_local_ns(InputContentDOM0, InputContentDOM),
   soap:soap_version(Version, SoapPrefix, ContentType),
   InputDOM = element(SoapPrefix:'Envelope', [],
            [ element(SoapPrefix:'Body', [], [InputContentDOM])
            ]),
   (   debugging(soap)
   ->  http_post_data(xml(ContentType, InputDOM),
            user_error, [])
   ;   true
   ),
   setup_call_cleanup(
       http_open(URL, In,
            [ method(post),
         post(xml(ContentType, InputDOM)),
         cert_verify_hook(cert_verify),
         status_code(Code),
         header(content_type, ReplyContentType)
            | SoapOptions
            ]),
       soap:soap_read_reply(Code, ReplyContentType, In, ReplyDOM),
       close(In)),
   ( memberchk(dom, Options) ->
     Reply = ReplyDOM
   ; soap:soap_reply(Code, SoapPrefix, ReplyDOM, OutputElements, M, Reply)).
