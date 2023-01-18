import TiledRawJSON from 'types/TiledRawJson'

export interface LevelConfigOptional {
  noPlayer: boolean
  playerChar: string
  objectLayers: string[]
  overlapLayers: string[]
  platformLayers: string[]
  npcLayers: string[]
  runConvos: boolean
  convosLayers: string[]
  zoom?: number
  itemLayers: string[]
  lights: boolean
  lightLayers: string[]
  playerStart: {
    x: number
    y: number
  }
  playerVisible: boolean
  playerPhysics: boolean
  runDialogue: boolean
  dialogueScript: string
  dialogueScriptFormat: string
  playMusic: boolean
  musicPath: string
  gameConfigOverrides: Phaser.Types.Core.GameConfig
  mapData: TiledRawJSON
  name: string
}

export interface LevelConfig extends Partial<LevelConfigOptional> {
  mapPath: string
}
