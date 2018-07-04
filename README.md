# library(race)

Prolog client for the SOAP interface of the [Attempto Reasoner RACE](http://attempto.ifi.uzh.ch/race/).

## Installation

This package requires [`library(wsdl)`](http://www.swi-prolog.org/pack/list?p=wsdl). It can be installed from within [SWI-Prolog](http://www.swi-prolog.org/) using the following command:

```prolog
?- pack_install(wsdl).
```

Then, you can install this package by calling:

```prolog
?- pack_install('https://github.com/fnogatz/race.git').
```

As of now, this package has not been published in SWI-Prolog's [packs list](http://www.swi-prolog.org/pack/list), so `pack_install(race)` is not yet supported.

## Example Usage

First of all you have to load the library:

```prolog
?- use_module(library(race)).
```

### Consistency Check

```prolog
%% check_consistency(+Knowledge)
?- check_consistency("John is great. He is not great.").
false.

%% check_consistency(+Knowledge, -Inconsistencies)
?- check_consistency("John is tall. Mary is tall. John is not tall.", Inconsistencies).
Inconsistencies = [fact("John is tall."), fact("John is not tall.")].
```

### Question Answering

```prolog
%% ask(+Knowledge, +Question, -Result)
%% ask(+Knowledge, +Question, -Result, +Options)
?- ask("Every man is a human. John is a man.", "Who is a human?", Result).
Result = results([
   proof([
      fact("Every man is a human."),
      fact("John is a man."),
      substitution("who", "John")
   ]),
   proof([
      fact("Every man is a human."),
      fact("John is a man."),
      substitution("who", "(at least 1) man")
   ])
]).
```

By using the option `sentence(true)`, it will return a textual response instead. It can be used in sentences like *this can be answered with ...*:

```prolog
?- ask("Every man is a human. John is a man.", "Who is a human?", Result, [sentence(true)]).
Result = results([
   "who as John and the known fact that Every man is a human and the known fact that John is a man",
   "who as (at least 1) man and the known fact that Every man is a human and the known fact that John is a man"
]).
```

### Theorem Proving

```prolog
%% prove(+Knowledge, +Theorem, -Result)
%% prove(+Knowledge, +Theorem, -Result, +Options)
?- prove("John likes Mary. If A likes B then B likes A.", "Mary likes John.", Result).
Result = results([
   proof([
      fact("John likes Mary."),
      fact("If A likes B then B likes A."),
      substitution("something", "John"),
      substitution("something", "Mary")
   ])
]).
```

Use `sentence(true)` in `Options` to get a textual response instead. It can be used in sentences like *this can be proven with ...*:

```prolog
?- prove("John likes Mary. If A likes B then B likes A.", "Mary likes John.", Result, [sentence(true)]).
Result = results([
   "something as John and something as Mary and the known fact that John likes Mary and the known fact that If A likes B then B likes A"
]).
```

## Background

This package uses the SOAP interface of the RACE web service. A [detailed specification](http://attempto.ifi.uzh.ch/site/docs/race_webservice.html) is available online as part of the RACE web service. We make use of the provided [WSDL file](http://attempto.ifi.uzh.ch/race_files/race.wsdl) with some modifications in order to be compatible with SWI-Prolog's `library(wsdl)`. The adapted and original version of the WSDL file can be found in the [`/prolog/race/wsdl` directory](https://github.com/fnogatz/race/blob/master/prolog/race/wsdl).
