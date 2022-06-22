#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate rs_poker;
use std::str;
use rs_poker::holdem::RangeParser;


fuzz_target!(|data: &[u8]| {
    if let Ok(s) = str::from_utf8(data) {
      if let Ok(h) = RangeParser::parse_one(s) {
        assert!(h.len() > 0);
      }
    }
});
