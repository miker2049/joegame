import 'phaser'
import { ITextWindow } from '../components/TextWindow'
import TextWindow from '../components/TextWindow'
interface TextWindowConfig {
    game: Phaser.Game
    x?: number
    y?: number
    width?: number
    height?: number
    text?: string
    additionalStyle?: string
}
export default function(config: TextWindowConfig): ITextWindow {
    return config.game.scene.add("textwindow",
        new TextWindow({ key: "textwindow_scene", physics: {} }),
        true,
        {
            x: config.x ?? config.game.renderer.width / 2,
            y: config.y ?? config.game.renderer.height / 2,
            width: config.width ?? 300,
            height: config.height ?? 300,
            text: config.text ?? '',
            additionalStyle: config.additionalStyle ?? ''
        }) as TextWindow;
}
