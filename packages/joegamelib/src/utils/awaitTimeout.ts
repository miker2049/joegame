export default async function(ms: number) {
    return new Promise(resolve => setTimeout(resolve, ms))
}
