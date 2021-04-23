//https://stackoverflow.com/a/7616484
export function hashToArr(str, amt) {
    let hash = 0, i, chr;
    if (str.length === 0)
        return [hash];
    for (i = 0; i < str.length; i++) {
        chr = str.charCodeAt(i);
        hash = ((hash << 7) - hash) + chr;
        hash |= 0; // Convert to 32bit integer
    }
    hash = Math.abs(hash);
    let hashStr = hash.toString();
    hashStr = hashStr + hashStr;
    // assert(hash.toString().length >= amt, 'hash is big enough')
    // console.log(hash, "HASH length")
    return [...hashStr.slice(amt * -1)].map((item) => Number(item));
}
;
//# sourceMappingURL=hashToArr.js.map