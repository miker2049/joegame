import { IUI } from "./IUI";

export class Menu extends Phaser.Scene implements IUI {

    constructor(){
        super({})
    }
    create() {
        this.add.rectangle(this.game.renderer.width / 2, this.game.renderer.height / 2,
            this.game.renderer.width * 0.75, this.game.renderer.height * 0.75,
                          0xffffff, 0.80)
        console.log('In menu')
    }


    show() {

    }

    hide() {

    }
}
