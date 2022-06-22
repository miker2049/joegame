#[macro_use]
extern crate criterion;
extern crate rand;
extern crate rs_poker;

use criterion::Criterion;
use rs_poker::holdem::StartingHand;

fn all_starting(c: &mut Criterion) {
    c.bench_function("Generate all starting hands", |b| b.iter(StartingHand::all));
}

fn iter_everything(c: &mut Criterion) {
    c.bench_function("Iter all possible hads from all starting hands", |b| {
        b.iter(|| -> usize {
            StartingHand::all()
                .iter()
                .map(|sh| -> usize { sh.possible_hands().len() })
                .sum()
        })
    });
}

criterion_group!(benches, all_starting, iter_everything);
criterion_main!(benches);
