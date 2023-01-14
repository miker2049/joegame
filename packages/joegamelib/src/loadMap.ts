import { createLevel } from './factories/createLevel'
import IjoegameFacade from './IjoegameFacade'
import { ILevelComponents } from './ILevel'
import { joegameFacade as fac } from './joegameFacade'
import { ILevelConfig } from './ILevelConfig'

import gameconfig from './gameconfig'
// @ts-ignore
const BASEURL_GLOBAL: string = BASEURL

export async function loadMap(
  config: ILevelConfig
): Promise<[ILevelComponents, IjoegameFacade]> {
  // const datastr = await (await fetch(BASEURL_GLOBAL+datapath)).text()
  // const data = parseCSVRowsToGameData(datastr)
  const game: Phaser.Game = await fac.initGame(BASEURL_GLOBAL, gameconfig)
  const level = await createLevel(game, config)

  return [level, fac]
}
