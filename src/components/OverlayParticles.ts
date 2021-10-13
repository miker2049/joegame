import createAnimatedParticleClass from "factories/createAnimatedParticleClass"

export interface ParticleOverlayConfig{
    scene: Phaser.Scene
    anim: Phaser.Animations.Animation
    x: number
    y: number
    texture: string
    frameEnd: number

}
export default class extends Phaser.GameObjects.Particles.ParticleEmitterManager{
    constructor(config: ParticleOverlayConfig){
        super(config.scene, config.texture)
        const pclass = createAnimatedParticleClass(config)
        this.createEmitter({
            particleClass: pclass
        })
    }
}
