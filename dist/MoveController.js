import 'phaser';
// import { ICharacterMoveMachine } from './ICharacter'
import { Dir } from './joegameTypes';
var Action;
(function (Action) {
    Action[Action["keydown"] = 0] = "keydown";
    Action[Action["keyup"] = 1] = "keyup";
})(Action || (Action = {}));
export default class MoveController {
    constructor(moveMachine, scene) {
        this.moveMachine = moveMachine;
        this.scene = scene;
        this.held = new Set();
        this.gameplayKeyDown = (event) => {
            if (event.repeat === true)
                return;
            if (this.shiftMapping[event.key] === undefined && this.mapping[event.key] === undefined)
                return;
            if (event.shiftKey && this.shiftMapping[event.key]) {
                this.shiftMapping[event.key]();
                return;
            }
            this.held.add(event.key);
            const ordered = [...this.held];
            const lastheld = ordered[ordered.length - 1];
            if (this.mapping[lastheld]) {
                this.mapping[lastheld]();
            }
        };
        this.gameplayKeyUp = (event) => {
            if (event.repeat === true)
                return;
            this.held.delete(event.key);
            const ordered = [...this.held];
            const lastheld = ordered[ordered.length - 1];
            if (lastheld != undefined && this.mapping[lastheld]) {
                // this.char.body.setVelocity(0,0);
                // this.char.alignOnTile();
                this.mapping[lastheld]();
            }
            else {
                // this.char.stopMove();
                this.moveMachine.send('STOP');
            }
        };
        this.gameplayMouseDown = () => {
            const point = this.scene.input.activePointer;
            if (point.leftButtonDown()) {
                this.moveMachine.send('MOVE_ON_PATH', { point: { x: point.worldX, y: point.worldY } });
            }
        };
        this.mapping = {
            ArrowLeft: () => {
                this.moveMachine.send('MOVE', { dir: Dir.west });
            },
            ArrowRight: () => {
                // this.char.move(Dir.east);
                this.moveMachine.send('MOVE', { dir: Dir.east });
            },
            ArrowUp: () => {
                // this.char.move(Dir.north);
                this.moveMachine.send('MOVE', { dir: Dir.north });
            },
            ArrowDown: () => {
                // this.char.move(Dir.south)
                this.moveMachine.send('MOVE', { dir: Dir.south });
            },
            a: () => {
                // this.char.move(Dir.south)
                // console.log(this.char.align());
            },
            // "d": this.mapping["ArrowRight"],
            // "w": this.mapping["ArrowUp"],
            // "s": this.mapping["ArrowDown"],
            // "h": this.mapping["ArrowLeft"],
            // "l": this.mapping["ArrowRight"],
            // "k": this.mapping["ArrowUp"],
            // "j": this.mapping["ArrowDown"],
            y: () => {
            },
            u: () => {
            },
            ' ': () => {
                // this.scene.initDialogue();
            }
        };
        this.shiftMapping = {
            ArrowLeft: () => {
                this.moveMachine.send('DASH', { dir: Dir.west });
            },
            ArrowRight: () => {
                this.moveMachine.send('DASH', { dir: Dir.east });
            },
            ArrowUp: () => {
                this.moveMachine.send('DASH', { dir: Dir.north });
            },
            ArrowDown: () => {
                this.moveMachine.send('DASH', { dir: Dir.south });
            },
            a: () => {
            },
            d: () => {
            },
            w: () => {
            },
            s: () => {
            },
            t: () => {
            },
            ' ': () => {
            }
        };
        this.setGameplayControl();
    }
    setGameplayControl() {
        this.scene.input.keyboard.on('keydown', this.gameplayKeyDown);
        this.scene.input.keyboard.on('keyup', this.gameplayKeyUp);
        this.scene.input.on('pointerdown', this.gameplayMouseDown);
        this.scene.input.on(Phaser.Input.Events.POINTER_WHEEL, ({ deltaY }) => {
            if (deltaY < 0) {
                const am = this.scene.cameras.main.zoom + 0.8;
                this.scene.cameras.main.zoomTo(am > 10 ? 10 : am);
            }
            else {
                const am = this.scene.cameras.main.zoom - 0.8;
                this.scene.cameras.main.zoomTo(am < 0.2 ? 0.2 : am);
            }
        });
    }
}
//# sourceMappingURL=MoveController.js.map