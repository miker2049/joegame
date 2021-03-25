export default function(n:number, x: number) {
    if(x>n) {
        return x
    }
    n = n + x/2
    n = n - (n%x)
    return n
}
