import 'phaser';
export default function (game: any, name: any, fragShader: string): {
    new (game: Phaser.Game): {
        onBoot(): void;
        onPreRender(): void;
        onDraw(renderTarget: Phaser.Renderer.WebGL.RenderTarget): void;
        gameObject: Phaser.GameObjects.GameObject;
        colorMatrix: Phaser.Display.ColorMatrix;
        fullFrame1: Phaser.Renderer.WebGL.RenderTarget;
        fullFrame2: Phaser.Renderer.WebGL.RenderTarget;
        halfFrame1: Phaser.Renderer.WebGL.RenderTarget;
        halfFrame2: Phaser.Renderer.WebGL.RenderTarget;
        copyFrame(source: Phaser.Renderer.WebGL.RenderTarget, target?: Phaser.Renderer.WebGL.RenderTarget | undefined, brightness?: number | undefined, clear?: boolean | undefined, clearAlpha?: boolean | undefined): void;
        copyToGame(source: Phaser.Renderer.WebGL.RenderTarget): void;
        drawFrame(source: Phaser.Renderer.WebGL.RenderTarget, target?: Phaser.Renderer.WebGL.RenderTarget | undefined, clearAlpha?: boolean | undefined): void;
        blendFrames(source1: Phaser.Renderer.WebGL.RenderTarget, source2: Phaser.Renderer.WebGL.RenderTarget, target?: Phaser.Renderer.WebGL.RenderTarget | undefined, strength?: number | undefined, clearAlpha?: boolean | undefined): void;
        blendFramesAdditive(source1: Phaser.Renderer.WebGL.RenderTarget, source2: Phaser.Renderer.WebGL.RenderTarget, target?: Phaser.Renderer.WebGL.RenderTarget | undefined, strength?: number | undefined, clearAlpha?: boolean | undefined): void;
        clearFrame(target: Phaser.Renderer.WebGL.RenderTarget, clearAlpha?: boolean | undefined): void;
        blitFrame(source: Phaser.Renderer.WebGL.RenderTarget, target: Phaser.Renderer.WebGL.RenderTarget, brightness?: number | undefined, clear?: boolean | undefined, clearAlpha?: boolean | undefined, eraseMode?: boolean | undefined): void;
        copyFrameRect(source: Phaser.Renderer.WebGL.RenderTarget, target: Phaser.Renderer.WebGL.RenderTarget, x: number, y: number, width: number, height: number, clear?: boolean | undefined, clearAlpha?: boolean | undefined): void;
        bindAndDraw(source: Phaser.Renderer.WebGL.RenderTarget, target?: Phaser.Renderer.WebGL.RenderTarget | undefined, clear?: boolean | undefined, clearAlpha?: boolean | undefined, currentShader?: Phaser.Renderer.WebGL.WebGLShader | undefined): void;
        name: string;
        game: Phaser.Game;
        renderer: Phaser.Renderer.WebGL.WebGLRenderer;
        manager: Phaser.Renderer.WebGL.PipelineManager;
        gl: WebGLRenderingContext;
        view: HTMLCanvasElement;
        width: number;
        height: number;
        vertexCount: number;
        vertexCapacity: number;
        readonly vertexData: ArrayBuffer;
        readonly vertexBuffer: WebGLBuffer;
        topology: number;
        bytes: Uint8Array;
        vertexViewF32: Float32Array;
        vertexViewU32: Uint32Array;
        active: boolean;
        currentUnit: number;
        forceZero: boolean;
        readonly hasBooted: boolean;
        readonly isPostFX: boolean;
        renderTargets: Phaser.Renderer.WebGL.RenderTarget[];
        currentRenderTarget: Phaser.Renderer.WebGL.RenderTarget;
        shaders: Phaser.Renderer.WebGL.WebGLShader[];
        currentShader: Phaser.Renderer.WebGL.WebGLShader;
        projectionMatrix: Phaser.Math.Matrix4;
        projectionWidth: number;
        projectionHeight: number;
        config: Phaser.Types.Renderer.WebGL.WebGLPipelineConfig;
        glReset: boolean;
        boot(): void;
        onResize(width: number, height: number): void;
        setShader(shader: Phaser.Renderer.WebGL.WebGLShader, setAttributes?: boolean | undefined): any;
        getShaderByName(name: string): Phaser.Renderer.WebGL.WebGLShader;
        setShadersFromConfig(config: Phaser.Types.Renderer.WebGL.WebGLPipelineConfig): any;
        setGameObject(gameObject: Phaser.GameObjects.GameObject, frame?: Phaser.Textures.Frame | undefined): number;
        shouldFlush(amount?: number | undefined): boolean;
        resize(width: number, height: number): any;
        setProjectionMatrix(width: number, height: number): any;
        updateProjectionMatrix(): void;
        bind(currentShader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        rebind(currentShader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        setVertexBuffer(): boolean;
        preBatch(gameObject?: Phaser.Cameras.Scene2D.Camera | Phaser.GameObjects.GameObject | undefined): any;
        postBatch(gameObject?: Phaser.Cameras.Scene2D.Camera | Phaser.GameObjects.GameObject | undefined): any;
        unbind(): void;
        flush(isPostFlush?: boolean | undefined): any;
        onActive(currentShader: Phaser.Renderer.WebGL.WebGLShader): void;
        onBind(gameObject?: Phaser.GameObjects.GameObject | undefined): void;
        onRebind(): void;
        onBatch(gameObject?: Phaser.GameObjects.GameObject | undefined): void;
        onPreBatch(gameObject?: Phaser.GameObjects.GameObject | undefined): void;
        onPostBatch(gameObject?: Phaser.GameObjects.GameObject | undefined): void;
        onRender(scene: Phaser.Scene, camera: Phaser.Cameras.Scene2D.Camera): void;
        onPostRender(): void;
        onBeforeFlush(isPostFlush?: boolean | undefined): void;
        onAfterFlush(isPostFlush?: boolean | undefined): void;
        batchVert(x: number, y: number, u: number, v: number, unit: number, tintEffect: number | boolean, tint: number): void;
        batchQuad(gameObject: Phaser.GameObjects.GameObject | null, x0: number, y0: number, x1: number, y1: number, x2: number, y2: number, x3: number, y3: number, u0: number, v0: number, u1: number, v1: number, tintTL: number, tintTR: number, tintBL: number, tintBR: number, tintEffect: number | boolean, texture?: WebGLTexture | undefined, unit?: number | undefined): boolean;
        batchTri(gameObject: Phaser.GameObjects.GameObject | null, x1: number, y1: number, x2: number, y2: number, x3: number, y3: number, u0: number, v0: number, u1: number, v1: number, tintTL: number, tintTR: number, tintBL: number, tintEffect: number | boolean, texture?: WebGLTexture | undefined, unit?: number | undefined): boolean;
        drawFillRect(x: number, y: number, width: number, height: number, color: number, alpha: number, texture?: WebGLTexture | undefined, flipUV?: boolean | undefined): void;
        setTexture2D(texture?: WebGLTexture | undefined): number;
        bindTexture(target?: WebGLTexture | undefined, unit?: number | undefined): any;
        bindRenderTarget(target?: Phaser.Renderer.WebGL.RenderTarget | undefined, unit?: number | undefined): any;
        setTime(name: string): any;
        set1f(name: string, x: number, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        set2f(name: string, x: number, y: number, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        set3f(name: string, x: number, y: number, z: number, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        set4f(name: string, x: number, y: number, z: number, w: number, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        set1fv(name: string, arr: number[] | Float32Array, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        set2fv(name: string, arr: number[] | Float32Array, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        set3fv(name: string, arr: number[] | Float32Array, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        set4fv(name: string, arr: number[] | Float32Array, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        set1iv(name: string, arr: number[] | Float32Array, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        set2iv(name: string, arr: number[] | Float32Array, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        set3iv(name: string, arr: number[] | Float32Array, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        set4iv(name: string, arr: number[] | Float32Array, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        set1i(name: string, x: number, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        set2i(name: string, x: number, y: number, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        set3i(name: string, x: number, y: number, z: number, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        set4i(name: string, x: number, y: number, z: number, w: number, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        setMatrix2fv(name: string, transpose: boolean, matrix: number[] | Float32Array, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        setMatrix3fv(name: string, transpose: boolean, matrix: Float32Array, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        setMatrix4fv(name: string, transpose: boolean, matrix: Float32Array, shader?: Phaser.Renderer.WebGL.WebGLShader | undefined): any;
        destroy(): any;
        shutdown(): void;
        eventNames(): (string | symbol)[];
        listeners(event: string | symbol): Function[];
        listenerCount(event: string | symbol): number;
        emit(event: string | symbol, ...args: any[]): boolean;
        on(event: string | symbol, fn: Function, context?: any): any;
        addListener(event: string | symbol, fn: Function, context?: any): any;
        once(event: string | symbol, fn: Function, context?: any): any;
        removeListener(event: string | symbol, fn?: Function | undefined, context?: any, once?: boolean | undefined): any;
        off(event: string | symbol, fn?: Function | undefined, context?: any, once?: boolean | undefined): any;
        removeAllListeners(event?: string | symbol | undefined): any;
    };
};
//# sourceMappingURL=createPostFXShader.d.ts.map