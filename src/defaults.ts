import { ILevelConfig } from "./ILevelConfig"

const levelConfig: ILevelConfig = {
    objectLayers: ['Objects', 'aboveObjects'],
    platformLayers: ['Platforms'],
    npcLayers: ['NPCs'],
    convosLayers: 'TweetConvos',
    zoom: 4,
}

export default {
    scale: 1,
    animLength: 3,
    dashDistance: 32,
    speed: 16,
    globalDrag: 1,
    tileWidth: 16,
    tileHeight: 16,
    charDepth: 2,
    platformDepth: 10,
    patience: 1000,
    charAccel: 128,
    talkingSpeed: 45,
    emoji: 'sweat',
    emojiPath: 'assets/images/emoji/',
    levelConfig
}
