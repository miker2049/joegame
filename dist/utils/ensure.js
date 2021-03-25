// https://stackoverflow.com/a/54738437
export default function (argument, message = 'This value was promised to be there.') {
    if (argument === undefined || argument === null) {
        throw new TypeError(message);
    }
    return argument;
}
//# sourceMappingURL=ensure.js.map