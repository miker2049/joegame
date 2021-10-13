interface ParticleConstructor {
    new (emitter: Phaser.GameObjects.Particles.ParticleEmitter): Phaser.GameObjects.Particles.Particle
}

export interface AnimatedParticleConfig {
    anim: Phaser.Animations.Animation
    frameEnd: number
    scene: Phaser.Scene
}

export default function(config: AnimatedParticleConfig): ParticleConstructor {

    const anim = config.anim
    const frameEnd = config.frameEnd

    return class extends Phaser.GameObjects.Particles.Particle {
        timeBuff: number
        currentFrame: number

        constructor(emitter: Phaser.GameObjects.Particles.ParticleEmitter) {
            super(emitter);

            this.timeBuff = 0;
            this.currentFrame = 0;
        }

        update(delta: number, step: number, processors: any) {
            var result = super.update(delta, step, processors);

            this.timeBuff += delta;

            if (this.timeBuff >= anim.msPerFrame) {
                this.currentFrame++;

                if (this.currentFrame > frameEnd) {
                    this.currentFrame = 0;
                }

                this.frame = anim.frames[this.currentFrame].frame;

                this.timeBuff -= anim.msPerFrame;
            }

            return result;
        }
    }
}
