import pako from "pako";

export function parseCompressed(input: string): number[] {
    try {
        const decoded = new Uint8Array(Buffer.from(input, "base64"));
        const result = pako.inflate(decoded);
        const arr = new Int32Array(result.buffer);
        const out = Array.from(arr);
        // console.log(out);
        return out;
    } catch (err) {
        console.log(err);
    }
}

export function compressData(d: number[]) {
    const arr = Int32Array.from(d);
    const res = pako.deflate(new Uint8Array(arr.buffer), { level: 9 });
    const b = Buffer.from(res.buffer);

    const out = b.toString("base64");
    console.log(out);
    return out;
}
// parseCompressed(
//     compressData(
//         parseCompressed(
//             "eAHtm+1twzAMRLNyu0RnaNoV+rFE96r44wHOoUkjx7ZI+wIIBP8o1PHlGCvI+XQ6vbX13tZrW2fnXXp8Nr2+2vpu66Mt5316mLfHPn/mrY831cv8mb+R/m3+zJ/5q/v9U+eJ8755bP+z/9n/7H9HfX62/9n/7H/2P/uf75/n3L/7eaPveUP18vz1/PX89fz1/PX89fzd/vdrz1/PX89fz1/PX89fz1/P32rfB/Q+wXnffUy1fmer17z18aZ6ZetntXpUT+d9PFbrd7Z6zVsfb6pXtn5Wq0f1dN7HY7V+Z6vXvPXxpnpl62f2evSORvV0fptH/Y9g9n5nqw/+iObtNm+qD/xFX1/aytbfbPXAmf5H+qdpFy/V1/klj/A2/Y0vuOOVrd/Z6oE/Ityhn3m75E31gL/o65Q79MvW79H1wNk1v0M3oup99BzervkduhFH9zvb+8MfUf0O3YhH503PD3/R17/8Dt2I2fq/dT1wdq/foRtR9T9aDm/3+h26Ebfud7b3gz/if36HbsSj8abnhb/o6z1+h27EbDysXQ+czfU7dCNqP/aew9tcv0O3iE9trd3vbPvDH7HX79AP7fbOm54P/qKvc/wu9Htua7qPesGecjjTMz3CXezJvtqfveVwspTfoR37ZvOnpeuBE+Ij3EVt7EPcG296niknS/gd+xGX7vfo/eBiLb/T82m/qudwsZbfqT6qZ/Uc/ohL+53qo3pWz+EvzrmG36k+qme1HM628jvVR/WslsPbVn6n+qie1XL4I67td6qP6lkth7841xZ+p/qontlzOBvld6qP6pk9h7dRfqf6qJ7Zc/gjbu13qo/qmT2HvzjHCL9TfX4B+jAyLQ=="
//         )
//     )
// );
