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

## Examples

```prolog
?- use_module(library(race)).
?- check_consistency('John is a man.').
?- check_consistency('John is a man.', Proof).
?- check_consistency('John is a man. John is not a man.', Proof).

?- ask('Every man is a human. John is a man.', 'Is John a human?', Proof, WhyNot).
?- ask('Every man is a human. John is a man.', 'Who is a human?', Proof, WhyNot).
```

## Background

This package uses the SOAP interface of the RACE web service. A [detailed specification]((http://attempto.ifi.uzh.ch/site/docs/race_webservice.html) is available online as part of the RACE web service. We make use of the provided [WSDL file](http://attempto.ifi.uzh.ch/race_files/race.wsdl) with some modifications in order to be compatible with SWI-Prolog's `library(wsdl)`. The adapted and original version of the WSDL file can be found in the [`/prolog/race/wsdl` directory](https://github.com/fnogatz/race/blob/master/prolog/race/wsdl).
