//! This module provides the ability to simulate a mutli table independent chip tournament.
//! It does this via simulation. Different heros and villans go to all in show downs. Then
//! the resulting placements are computed as each player busts.
//!
//! This method does not require a recursive dive N! so it makes simulating
//! tournaments with many different people and different payments feasible. However it comes with
//! some downsides.
//!
//! - The results are not repeatable.
//! - Small SNG's would be faster to compute with full ICM rather than simulations
//!
//! However it does have some other nice properties
//!
//! - It's parrallelizable. This can be farmed out to many different cores to speed
//! this up. Since each tournament is indepent there's little coordination oeverhead needed.
//! - We can change the players skill easily. Since ICM just looks at the percentage or outstanding chips
use fixedbitset::FixedBitSet;
use rand::seq::IteratorRandom;
use rand::{thread_rng, Rng};

const DEFAULT_PAYMENT: i32 = 0;

#[inline]
fn award_payments(
    remaining_stacks: &[i32],
    payments: &[i32],
    idx: usize,
    other_idx: usize,
    winnings: &mut Vec<i32>,
    next_place: &mut usize,
) -> bool {
    if remaining_stacks[idx] == 0 {
        winnings[idx] += payments.get(*next_place).unwrap_or(&DEFAULT_PAYMENT);
        *next_place -= 1;
        if *next_place == 0 {
            winnings[other_idx] += payments.get(*next_place).unwrap_or(&DEFAULT_PAYMENT);
        }
        true
    } else {
        false
    }
}

pub fn simulate_icm_tournament(chip_stacks: &[i32], payments: &[i32]) -> Vec<i32> {
    // We're going to mutate in place so move the chip stacks into a mutable vector.
    let mut remaining_stacks: Vec<i32> = chip_stacks.into();
    // Thread local rng.
    let mut rng = thread_rng();
    // Which place in the next player to bust will get.
    let mut next_place = remaining_stacks.len() - 1;

    // The results.
    let mut winnings = vec![0; remaining_stacks.len()];
    let mut remaining_players = FixedBitSet::with_capacity(remaining_stacks.len());

    // set all the players as still having chips remaining.
    remaining_players.insert_range(..);

    while next_place > 0 {
        // Perform a choose multiple. We do this random choice rather than iterating because
        // we really don't want order to be the deciding factor. I am assuming the when
        // ICM is important that most players will make push/fold decisions based upon
        // their hole cards.
        if let [hero, villan] = remaining_players
            .ones()
            .choose_multiple(&mut rng, 2)
            .as_slice()
        {
            // For now assume that each each player has the same skill.
            // TODO: Check to see if adding in a skill(running avg of win %) array for each player is needed.
            let hero_won: bool = rng.gen_bool(0.5);

            // can't bet chips that can't be called.
            let effective_stacks =
                std::cmp::min(remaining_stacks[*hero], remaining_stacks[*villan]);
            let hero_change: i32 = if hero_won {
                effective_stacks
            } else {
                -effective_stacks
            };
            remaining_stacks[*hero] += hero_change;
            remaining_stacks[*villan] -= hero_change;

            // Check if hero was eliminated.
            if award_payments(
                &remaining_stacks,
                &payments,
                *hero,
                *villan,
                &mut winnings,
                &mut next_place,
            ) {
                remaining_players.set(*hero, false);
            }

            // Now check if the villan was eliminated.
            if award_payments(
                &remaining_stacks,
                &payments,
                *villan,
                *hero,
                &mut winnings,
                &mut next_place,
            ) {
                remaining_players.set(*villan, false);
            }
        }
    }
    winnings
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_huge_lead_wins() {
        let stacks = vec![1000, 2, 1];
        let payments = vec![100, 30, 10];

        let mut total_winnings = vec![0; 3];
        let num_trials = 1000;

        for _i in 0..num_trials {
            let single_wins = simulate_icm_tournament(&stacks, &payments);
            total_winnings = total_winnings
                .iter()
                .zip(single_wins.iter())
                .map(|(a, b)| a + b)
                .collect()
        }

        let final_share: Vec<f64> = total_winnings
            .iter()
            .map(|v| (*v as f64) / (num_trials as f64))
            .collect();

        assert!(
            final_share[0] > final_share[1],
            "The total winnings of a player with most of the chips should be above the rest."
        );
        dbg!(final_share);
    }

    #[test]
    fn about_same() {
        let stacks = vec![1000, 1000, 999];
        let payments = vec![100, 30, 10];

        let mut total_winnings = vec![0; 3];
        let num_trials = 1000;

        for _i in 0..num_trials {
            let single_wins = simulate_icm_tournament(&stacks, &payments);
            total_winnings = total_winnings
                .iter()
                .zip(single_wins.iter())
                .map(|(a, b)| a + b)
                .collect()
        }

        let final_share: Vec<f64> = total_winnings
            .iter()
            .map(|v| (*v as f64) / (num_trials as f64))
            .collect();

        let sum: f64 = final_share.iter().sum();
        let avg = sum / (final_share.len() as f64);

        for &share in final_share.iter() {
            assert!(share < 1.1 * avg);
            assert!(1.1 * share > avg);
        }

        dbg!(final_share);
    }
}
