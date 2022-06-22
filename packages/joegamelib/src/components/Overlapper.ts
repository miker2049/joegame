interface OverlapperConfig {
    scene: Phaser.Scene
    checker: ()=>boolean
    enterCB: ()=>void
    exitCB: ()=>void
    active: true
}
export class Overlapper {

    deltabuff: number = 0
    overlaptmp: boolean = false
    overlapped: boolean = false
    active: boolean


    checkOverlap: ()=>boolean
    enterCallback: ()=>void
    exitCallback: ()=>void

    constructor(config: OverlapperConfig){
        this.checkOverlap = config.checker
        this.enterCallback = config.enterCB
        this.exitCallback = config.exitCB
        this.active = config.active
        config.scene.events.on('update', this.updateCallback.bind(this))
    }
    updateCallback(_sys: Phaser.Scenes.Systems, delta: number) {
        if(!this.active) return

        this.deltabuff += delta

        if (this.deltabuff>1000/10) {
            this.deltabuff = 0

            this.overlaptmp = this.checkOverlap()
            if (
                this.overlaptmp && !this.overlapped
            ) {
                this.overlapped = true
                this.enterCallback()
            } else if (
                !this.overlaptmp && this.overlapped
            ) {
                this.overlapped = false
                this.exitCallback()
            }

        }
    }

}
