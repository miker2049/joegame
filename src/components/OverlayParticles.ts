import createAnimatedParticleClass from "factories/createAnimatedParticleClass"

export interface ParticleOverlayConfig {
    scene: Phaser.Scene
    anim: Phaser.Animations.Animation
    x: number
    y: number
    texture: string
    frameEnd: number
    follow: Phaser.GameObjects.GameObject
}

export default class extends Phaser.GameObjects.Particles.ParticleEmitterManager {
    constructor(config: ParticleOverlayConfig) {
        super(config.scene, config.texture)
        const pclass = createAnimatedParticleClass(config)
        this.setDepth(100)

        this.createEmitter({
            // TODO ugly cast, related to #5731 in phaser github
            particleClass: pclass as unknown as Phaser.GameObjects.Particles.Particle,
            lifespan: 8000,
            speed: { min: 5, max: 50 },
            follow: config.follow,
            angle: { min: 0, max: 180 },
            scale: { min: 0.2, max: 0.7 },
            followOffset: {
                y: -200
            }
        })
    }
}
