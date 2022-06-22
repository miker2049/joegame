#[macro_use]
extern crate criterion;
extern crate rand;
extern crate rs_poker;

use criterion::Criterion;
use rs_poker::holdem::RangeParser;

fn parse_ako(c: &mut Criterion) {
    c.bench_function("Parse AKo", |b| {
        b.iter(|| RangeParser::parse_one("AKo"));
    });
}

fn parse_pairs(c: &mut Criterion) {
    c.bench_function("Parse pairs (22+)", |b| {
        b.iter(|| RangeParser::parse_one("22+"));
    });
}

fn parse_connectors(c: &mut Criterion) {
    c.bench_function("Parse connectors (32+)", |b| {
        b.iter(|| RangeParser::parse_one("32+"));
    });
}

fn parse_plus(c: &mut Criterion) {
    c.bench_function("Parse plus (A2+)", |b| {
        b.iter(|| RangeParser::parse_one("A2+"));
    });
}

criterion_group!(
    benches,
    parse_ako,
    parse_pairs,
    parse_connectors,
    parse_plus
);
criterion_main!(benches);
