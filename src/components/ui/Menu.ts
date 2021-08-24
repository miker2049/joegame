import { TextWindow } from "components/TextWindow";
import { IUI } from "./IUI";

export class Menu extends TextWindow implements IUI {
    title: string
    menuItems: string[]

    constructor(menuItems: string[], title: string){
        super({})
        this.menuItems = menuItems
        this.title = title
    }
    create() {
        this.phaserDom.setOrigin(0.5)
        this.x = this.game.renderer.width/2
        this.y = this.game.renderer.height/2
        this.appendStyle('background-color', 'gray')
        // this.appendStyle('padding', '0.03em')
        // this.appendStyle('margin', '0.03em')

        this.constructMenu(0)
        // this.phaserDom.node.getElementsByTagName('ul')[0].style.listStyle = "none"
    }

    constructMenu(selected: number){
        let html = this.menuItems.reduce((acc, item, curr)=>
            acc += `${curr===selected ? '>' : '&nbsp;&nbsp;'} ${item} <br>`,'<br>')

        this.setMDText(` # ${this.title} \n ${html}`)
    }

    // show() {

    // }

    // hide() {

    // }
}


export function createMenu(scene: Phaser.Scenes.SceneManager): Menu {

    return scene.add('menu',new Menu(["new game","load", "settings","blog","wiki"],"joegame"), true) as Menu
}
