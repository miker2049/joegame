export default function (n, x) {
    if (x > n) {
        return x;
    }
    n = n + x / 2;
    n = n - (n % x);
    return n;
}
//# sourceMappingURL=closestMultiple.js.map