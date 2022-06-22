#[macro_use]
extern crate criterion;
extern crate rand;
extern crate rs_poker;

use criterion::Criterion;
use rs_poker::core::{Deck, Flattenable, Hand, Rankable};

fn rank_one(c: &mut Criterion) {
    let d = Deck::default().flatten();
    let hand = Hand::new_with_cards(d.sample(5));
    c.bench_function("Rank one 5 card hand", move |b| b.iter(|| hand.rank()));
}

fn rank_best_seven(c: &mut Criterion) {
    let d = Deck::default().flatten();
    let hand = Hand::new_with_cards(d.sample(7));
    c.bench_function("Rank best 5card hand from 7", move |b| {
        b.iter(|| hand.rank())
    });
}

criterion_group!(benches, rank_one, rank_best_seven);
criterion_main!(benches);
