#[macro_use]
extern crate criterion;
extern crate rs_poker;

use criterion::{Bencher, Criterion};
use rs_poker::simulated_icm::simulate_icm_tournament;

use rand::{thread_rng, Rng};

fn simulate_one_tournament(c: &mut Criterion) {
    let payments = vec![10_000, 6_000, 4_000, 1_000, 800];
    let mut rng = thread_rng();
    let num_players: Vec<usize> = vec![2, 3, 4, 6, 128, 256];
    c.bench_function_over_inputs(
        "Simulate random chipped players",
        move |b: &mut Bencher, num_players: &usize| {
            let chips: Vec<i32> = (0..*num_players).map(|_pn| rng.gen_range(1, 500)).collect();
            b.iter(|| simulate_icm_tournament(&chips, &payments))
        },
        num_players,
    );
}

criterion_group!(benches, simulate_one_tournament);
criterion_main!(benches);
