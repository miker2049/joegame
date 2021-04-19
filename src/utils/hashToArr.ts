//https://stackoverflow.com/a/7616484
export default function(str: string, amt: number): number[] {
    let hash = 0, i, chr;
    if (str.length === 0) return [hash];
    for (i = 0; i < str.length; i++) {
        chr = str.charCodeAt(i);
        hash = ((hash << 5) - hash) + chr;
        hash |= 0; // Convert to 32bit integer
    }
    return [...hash.toString().slice(amt * -1)].map((item) => Number(item));
};
