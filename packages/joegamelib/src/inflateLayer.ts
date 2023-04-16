import {
  ILayer,
  IObjectLayer,
  ITileLayer,
  ITileLayerInflated
} from './types/TiledRawJson'
import { unzlib } from 'fflate'

async function parseCompressed(input: string): Promise<number[]> {
  try {
    const d = Uint8Array.from(atob(input), (c) => c.charCodeAt(0))
    const result = await new Promise<Uint8Array>((res, rej) => {
      unzlib(d, {}, (err, f) => {
        if (err) rej(err)
        res(f)
      })
    })
    const arr = new Int32Array(result.buffer)
    const out = Array.from(arr)
    return out
  } catch (err) {
    throw Error('Error parsing compressed layers:  ' + err)
  }
}

/**
 * Returns a layer where, if the input is a tilelayer
 * with a string-type data prop, the layer returned is the
 * decompressed value of that data as `number[]`.  Otherwise,
 * just return back the layer.
 */
export async function inflateLayer(
  l: ILayer
): Promise<ITileLayerInflated | IObjectLayer> {
  if (l.type === 'objectgroup') return l
  else if (typeof l.data !== 'string') return l as ITileLayerInflated
  else
    return {
      ...l,
      data: await parseCompressed(l.data),
      encoding: 'csv',
      compression: undefined
    }
}
