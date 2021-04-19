// https://dl.acm.org/doi/10.1145/10563.10583
//  Each vowel (a, e, i, o, u, y) in a word counts as one syllable
//  subject to the following sub-rules:
// - Ignore final -ES, -ED, E (except for -LE)
// - Words of three letters or less count as one syllable
// - Consecutive vowels count as one syllable.
//
// https://stackoverflow.com/questions/5686483/how-to-compute-number-of-syllables-in-a-word-in-javascript
export default function(word: string): number {
    word = word.toLowerCase();
    if (word.length <= 3) { return 1; }
    word = word.replace(/(?:[^laeiouy]es|ed|[^laeiouy]e)$/, '');
    word = word.replace(/^y/, '');
    return word.match(/[aeiouy]{1,2}/g)?.length ?? 1;
}
