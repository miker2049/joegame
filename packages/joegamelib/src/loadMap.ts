import { createLevel } from './factories/createLevel'
import IjoegameFacade from './IjoegameFacade'
import { ILevelComponents } from './ILevel'
import { joegameFacade as fac } from './joegameFacade'
import { LevelConfig } from './ILevelConfig'

import gameconfig from './gameconfig'
// @ts-ignore
const BASEURL_GLOBAL: string = BASEURL

export async function loadMap(
  config: LevelConfig
): Promise<[ILevelComponents, IjoegameFacade]> {
  // const datastr = await (await fetch(BASEURL_GLOBAL+datapath)).text()
  // const data = parseCSVRowsToGameData(datastr)
  const finalGameConfig = config.gameConfigOverrides
    ? Object.assign(gameconfig, config.gameConfigOverrides)
    : gameconfig
  const game: Phaser.Game = await fac.initGame(BASEURL_GLOBAL, finalGameConfig)
  const level = await createLevel(game, config)

  return [level, fac]
}
