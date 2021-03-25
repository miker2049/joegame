import TweetConvo from '../components/TweetConvo'
import createTweetConvo from '../factories/createTweetConvo'
import { ILevelComponents } from '../ILevel'
import shuffle from '../utils/shuffleArr'

export default async function(level: ILevelComponents, layer: string): Promise<TweetConvo[] | undefined> {
    if (!level.map.getObjectLayer(layer)) { return }
    let convos: TweetConvo[] = []

    let mani = JSON.parse(JSON.stringify(level.scene.cache.json.get('convo-manifest')))
    for await (let obj_ of level.map.getObjectLayer(layer).objects) {
        let convoIDD: string
        // const coord = level.map.tileToWorldXY(obj_.x, obj_.y)
        const charGroup = obj_.properties?.find(prop => prop.name === 'charGroup')?.value ?? 'all'
        if (mani.length > 0) {
            convoIDD = shuffle(mani).pop()
        } else {
            mani = JSON.parse(JSON.stringify(level.scene.cache.json.get('convo-manifest')))
            mani = mani.files as string[]
            convoIDD = shuffle(mani).pop()
        }
        convos.push(await createTweetConvo(level, obj_.x, obj_.y, charGroup, convoIDD))
    }
    return convos
}
