extern crate rs_poker;
use std::iter::FromIterator;

use std::vec::Vec;

use rs_poker::core::{Card, CardIter, Deck, FlatDeck, Flattenable, Hand, Rank, Rankable};
use rs_poker::holdem::{MonteCarloGame, RangeParser};

fn main() {
    // let hands: Vec<Hand> = ["Adkh", "8c8s"]
    //     .iter()
    //     .map(|s| Hand::new_from_str(s).expect("Should be able to create a hand."))
    //     .collect();
    // let board = Hand::new_from_str("2d3h7c6hTc").expect("Should be able to create a hand.");

    let game = make_random_game();
    let game_ref = game.clone();
    let mut deck_i = Deck::default();
    for hand in &game.0{
        for card in hand.iter() {
            deck_i.remove(*card);
        }
    }
    for card in game.1.iter() {
        deck_i.remove(*card);
    }
    let deck_f: FlatDeck = deck_i.into();
    let it = CardIter::new(&deck_f[..], 5);
    for ps in it{
        println!("hmmmm {:?}", ps);
    }
    // println!("the hands are  {:?}", &game.0);
    // println!("the board is {:?}", &game.1);
    let mut g = MonteCarloGame::new_with_hands(game.0,game.1).expect("Should be able to create a game.");
    let r = g.simulate().expect("There should be one best rank.");
    // println!("{}",)
    println!("board is: {:}", game_ref.1.iter().fold(String::new(), |acc,e|{
        acc + &e.to_string() + "-"
    }));
    println!("winner is: {:}, with a {:?}", game_ref.0.get(r.0).unwrap(), r.1);

    println!("hands were:  ");
    for i in 0..game_ref.0.len() {
        println!("{:}: {:}", i, game_ref.0.get(i).unwrap());
    }
    // println!("{}", RangeParser::parse_one("77-44").unwrap().iter().fold(String::new(), |acc,x| acc+&x.to_str()))
}

fn make_random_game() -> (Vec<Hand>,Vec<Card>) {
    let mut deck: FlatDeck = Deck::default().into();
    let mut hands = [Hand::default(), Hand::default()];
    deck.shuffle();
    for _ in 0..2 {
        hands[0].push(deck.deal().expect("there is a card to grab"));
        hands[1].push(deck.deal().expect("there is a card to grab"));
    }
    let mut board = Vec::new();
    for _ in 0..5 {
        board.push(deck.deal().expect("there is a card to grab"));
    }
    (hands.to_vec(),board)
}
