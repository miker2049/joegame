import createLevel from 'factories/createLevel'
import IjoegameFacade from 'IjoegameFacade'
import { ILevelComponents } from "ILevel"
import joegameFacade from "joegameFacade"
import { ILevelConfig } from './ILevelConfig'

// @ts-ignore
const BASEURL_GLOBAL: string = BASEURL

export async function loadMap(mapjsonfile: string,
    baseURL: string,
    datapath: string,
    plyr: { x: number, y: number },
    lvlCfg?: ILevelConfig): Promise<[ILevelComponents, IjoegameFacade]> {

    const fac = new joegameFacade()
    // const datastr = await (await fetch(BASEURL_GLOBAL+datapath)).text()
    // const data = parseCSVRowsToGameData(datastr)
    const game: Phaser.Game = await fac.initGame(BASEURL_GLOBAL)
    const level = await createLevel(mapjsonfile, fac, game, plyr, lvlCfg)

    return [level, fac]
}
