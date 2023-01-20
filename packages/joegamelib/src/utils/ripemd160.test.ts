import test from 'tape'
import { jprng, xyhash } from './ripemd160'

test('joegame prng is deterministic based on input', (t) => {
  t.equal(jprng(1, 2), jprng(1, 2))
  t.equal(jprng(1, 2, 420), jprng(1, 2, 420))
  t.equal(jprng(4, 2), jprng(4, 2))
  t.equal(jprng(3, 2, 2), jprng(3, 2, 2))
  t.end()
})

test('joegame prng gives nice distribution', (t) => {
  const n = 100
  let sum = 0
  for (let y = 0; y < n; y++) {
    for (let x = 0; x < n; x++) {
      sum += jprng(x, y)
    }
  }
  const avg = sum / (n * n)
  t.assert(avg >= 0.49)
  t.assert(avg <= 0.51)
  t.end()
})
