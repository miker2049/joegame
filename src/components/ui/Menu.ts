import { TextWindow } from "components/TextWindow";
import { createLevel } from "factories/createLevel";
import { IUI } from "./IUI";

export class Menu extends TextWindow implements IUI {
    title: string
    menuItems: string[]
    currSelect: number = 0

    constructor(menuItems: string[], title: string) {
        super({})
        this.menuItems = menuItems
        this.title = title
    }
    create() {
        this.phaserDom.setOrigin(0.5)
        this.x = this.game.renderer.width / 2
        this.y = this.game.renderer.height / 2
        this.appendStyle('background-color', 'gray')
        // this.appendStyle('padding', '0.03em')
        // this.appendStyle('margin', '0.03em')

        console.log('in create cb of Menu')
        this.constructMenu(this.currSelect)

        this.input.keyboard.on('keydown-DOWN', () => {
            this.currSelect = Math.min(this.currSelect + 1, this.menuItems.length - 1)
            console.log(this.currSelect);
            this.constructMenu(this.currSelect)
        })
        this.input.keyboard.on('keydown-UP', () => {
            this.currSelect = Math.max(this.currSelect - 1, 0)
            console.log(this.currSelect);
            this.constructMenu(this.currSelect)
        })
        this.input.keyboard.on('keydown-ENTER', () => {
            createLevel('assets/maps/desert_bridge.json',  this.game, { x: 32, y: 32 })
        })
        // this.scene.input.keyboard.on('keyup', this.gameplayKeyUp)
        // this.phaserDom.node.getElementsByTagName('ul')[0].style.listStyle = "none"
    }

    constructMenu(selected: number) {
        let html = this.menuItems.reduce((acc, item, curr) =>
            acc += `${curr === selected ? '>' : '&nbsp;&nbsp;'} ${item} <br>`, '<br>')

        this.setMDText(` # ${this.title} \n ${html}`)
    }

    // show() {

    // }

    // hide() {

    // }
}


export function createMenu(scene: Phaser.Scenes.SceneManager): Menu {

    return scene.add('menu', new Menu(["new game", "load", "settings", "blog", "wiki"], "joegame"), true) as Menu
}
