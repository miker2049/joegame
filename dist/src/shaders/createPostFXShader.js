import 'phaser';
/*
 * Aiming to be used right now to
 */
export default function (game, name, fragShader) {
    return class extends Phaser.Renderer.WebGL.Pipelines.PostFXPipeline {
        constructor(game) {
            super({
                game,
                name,
                fragShader
            });
        }
        onBoot() {
            // this.setTexture();
        }
        onPreRender() {
            this.set1f('iTime', this.game.loop.time / 1000);
        }
        onDraw(renderTarget) {
            this.set2f('iResolution', renderTarget.width, renderTarget.height);
            this.bindAndDraw(renderTarget);
        }
    };
}
//# sourceMappingURL=createPostFXShader.js.map