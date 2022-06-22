# rs-poker

[![Crates.io](https://img.shields.io/crates/v/rs-poker.svg)](https://crates.io/crates/rs-poker)

RS Poker is a rust library aimed at being a good starting place
for lots of poker rust code. Correctness and performance are the two main goals.

Documentation is uploaded [here](https://docs.rs/rs_poker)
The crates.io page is [here](https://crates.io/crates/rs_poker)

## Core

The Core module contains code that is not specific to different
types of poker games. It contains:

* Suit type
* Value type
* Card type
* Deck
* Hand iteration
* Poker hand rank type
* Poker hand evaluation for five card hands.
* Poker hand evaluation for seven card hands.

The poker hand (5 card) evaluation will rank a hand in ~20 nanoseconds
per hand. That means that 50 Million hands per second can be
ranked. The seven card hand evaluation will rank a hand in < 25 ns.

The hand evaluation is is fully accurate, it does not rely on just single
kicker. This allows for breaking ties on hands that are closer.


## Holdem

The holdem module contains code that is specific to holdem. It
currently contains:

* Starting hand enumeration
* Hand range parsing
* Monte Carlo game simulation helpers.

## Testing

The code is pretty well tested and benchmarked. If you find
something that looks like a bug please submit a pr with test
code.

5 Card + Hand iteration has been used in conjunction with fuzzing to validate
the seven card hand evaluation.
