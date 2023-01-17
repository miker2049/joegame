export interface ILevelConfigOptional {
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
}

export interface ILevelConfig extends Partial<ILevelConfigOptional> {
  mapPath: string
}
