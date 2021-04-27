import 'phaser'

/*
 * Aiming to be used right now to
 */
export default function(game: Phaser.Game, name: string, fragShader: string) {
    return class extends Phaser.Renderer.WebGL.Pipelines.PostFXPipeline {

        constructor(game: Phaser.Game) {
            super({
                game,
                name,
                fragShader
            });
        }

        onBoot(): void {
            // this.setTexture();
        }

        onPreRender() {
            this.set1f('iTime', this.game.loop.time / 1000);
        }

        onDraw(renderTarget: Phaser.Renderer.WebGL.RenderTarget): void {

            this.set2f('iResolution', renderTarget.width, renderTarget.height);
            this.bindAndDraw(renderTarget);
        }
    }
}
