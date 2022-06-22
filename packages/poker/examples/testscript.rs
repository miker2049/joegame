extern crate rs_poker;
use num::FromPrimitive;
use rs_poker::{holdem::MonteCarloGame};
use rs_poker::core::Deck;
use rs_poker::core::Flattenable;
use num::BigInt;
use rs_poker::core::{Hand, Rankable};

fn main() {

    let hand_str = String::from("2h2d8d8sAd6sTh");
    let hand_str2 = String::from("2h2d8d8s3d6sTh");
    let hand = Hand::new_from_str(&hand_str).unwrap();
    let hand2 = Hand::new_from_str(&hand_str2).unwrap();
    let rank = hand.rank();
    let rank2 = hand2.rank();
    println!("{:?}", &hand_str);
    println!("{:?}", &rank);
    println!("{:?}", &hand_str2);
    println!("{:?}", &rank2);
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
