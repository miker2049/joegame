extern crate rs_poker;
use rs_poker::core::{Card, CardIter, Hand, Suit, Value};
use rs_poker::holdem::MonteCarloGame;

fn main(){
    sim(80_000);
}
fn sim(time: u64) {
    let hands = ["Adkh", "8c8s"]
        .iter()
        .map(|s| Hand::new_from_str(s).expect("Should be able to create a hand."))
        .collect();
    let mut g =
        MonteCarloGame::new_with_hands(hands, vec![
            Card {
            value: Value::Three,
            suit: Suit::Spade
            },
            Card {
            value: Value::Four,
            suit: Suit::Heart
            },
        ]).expect("Should be able to create a game.");

    let mut wins: [u64; 2] = [0, 0];

    for _ in 0..time {
        let r = g.simulate().expect("There should be one best rank.");
        g.reset();
        wins[r.0] += 1
    }
    println!("Wins (with {:} simultations) = {:.4}%", time, (wins[0] as f64/time as f64) * 100.0);
}
