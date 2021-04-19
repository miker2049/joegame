"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

// https://dl.acm.org/doi/10.1145/10563.10583
//  Each vowel (a, e, i, o, u, y) in a word counts as one syllable
//  subject to the following sub-rules:
// - Ignore final -ES, -ED, E (except for -LE)
// - Words of three letters or less count as one syllable
// - Consecutive vowels count as one syllable.
//
// https://stackoverflow.com/questions/5686483/how-to-compute-number-of-syllables-in-a-word-in-javascript
function _default(word) {
  var _word$match$length, _word$match;

  word = word.toLowerCase();

  if (word.length <= 3) {
    return 1;
  }

  word = word.replace(/(?:[^laeiouy]es|ed|[^laeiouy]e)$/, '');
  word = word.replace(/^y/, '');
  return (_word$match$length = (_word$match = word.match(/[aeiouy]{1,2}/g)) === null || _word$match === void 0 ? void 0 : _word$match.length) !== null && _word$match$length !== void 0 ? _word$match$length : 1;
}
//# sourceMappingURL=syllableCount.js.map