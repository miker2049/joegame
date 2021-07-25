extern crate rs_poker;
use num::FromPrimitive;
use rs_poker::core::Hand;
use rs_poker::holdem::MonteCarloGame;
use rs_poker::core::Deck;
use rs_poker::core::Flattenable;
use num::BigInt;


fn main() {
    // let hands = ["Adkh", "8c8s"]
    //     .iter()
    //     .map(|s| Hand::new_from_str(s).expect("Should be able to create a hand."))
    //     .collect();
    let mut deck = Deck::default().flatten();
    deck.shuffle();
    let card = deck.deal().unwrap();
    // println!("Wins = {:?}", fact(52));
    println!("Wins = {:?}", combinations(52,11));
}

fn fact(n:u128)->BigInt{
    let mut out = BigInt::from_u16(1).unwrap();
    for i in 1..n+1 {
        out = out * i;
    }
    out
}

fn combinations(n:u128, r:u128)->String{
    (fact(n)/(fact(r)*fact(n-r))).to_string()
}
