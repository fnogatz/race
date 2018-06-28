:- module(race, [
      check_consistency/1,
      check_consistency/2,
      ask/4
   ]).

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

check_consistency(Knowledge, Proof) :-
   Operation = ('http://attempto.ifi.uzh.ch/race':'RacePortType') /
               ('http://attempto.ifi.uzh.ch/race':'RunRace'),
   soap_call(Operation, [
      'Axioms'=Knowledge,
      'Mode'='check_consistency'
      %, 'Parameter'='raw'
   ], [ReplyDOM], [dom]),
   ( xpath(ReplyDOM, //(_:'Proof'), ProofDOM) ->
     proof(ProofDOM, Proof)
   ; Proof = [] ),
   !.

ask(Knowledge, Question, Proof, WhyNot) :-
   Operation = ('http://attempto.ifi.uzh.ch/race':'RacePortType') /
               ('http://attempto.ifi.uzh.ch/race':'RunRace'),
   soap_call(Operation, [
      'Axioms'=Knowledge,
      'Mode'='answer_query',
      'Theorems'=Question
      %, 'Parameter'='raw'
   ], ReplyDOM, [dom]),
   ( xpath(ReplyDOM, //(_:'WhyNot'), WhyNotDOM) ->
     why_not(WhyNotDOM, WhyNot)
   ; WhyNot = [] ),
   ( xpath(ReplyDOM, //(_:'Proof'), ProofDOM) ->
     proof(ProofDOM, Proof)
   ; Proof = [] ),
   !.

proof(ProofDOM, Proof) :-
   findall(Text, (xpath(ProofDOM, //(_:'Axiom'), element(_, _, [Text]))), Texts),
   remove_enumerates(Texts, Proof).

remove_enumerates(Texts, Proof) :-
   maplist(remove_enumerate, Texts, Proof).

remove_enumerate(Text, P) :-
   string_codes(Text, Codes),
   string_codes(': ', ColonList),
   append(ColonList, Rest, RestWithColonList),
   append(_LineNumberList, RestWithColonList, Codes),
   string_codes(P, Rest).

why_not(element(_, _, []), []) :- !.
why_not(_, [whynot]).


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

% just for debugging purposes
pp(Term) :-
   print_term(Term, []).
