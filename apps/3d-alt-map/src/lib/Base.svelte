<script>
  import {
    Canvas,
    Scene,
    PerspectiveCamera,
    DirectionalLight,
    //BasicShadowMap,
    //PCFShadowMap,
    PCFSoftShadowMap,
    //VSMShadowMap,
    AmbientLight,
    BoxBufferGeometry,
    PlaneBufferGeometry,
    Mesh,
    MeshStandardMaterial,
    WebGLRenderer,
    OrbitControls,
    DoubleSide,
    MathUtils,
  } from "svelthree";
import RandomGrid from "./RandomGrid.svelte";


  let floorGeometry = new PlaneBufferGeometry(16, 16, 1);
  let floorMaterial = new MeshStandardMaterial();

</script>

<Canvas let:sti w={500} h={500}>
    <Scene {sti} let:scene id="scene1" props={{ background: 0xedf2f7 }}>
        <PerspectiveCamera
            {scene}
            id="cam1"
            pos={[0, 0, 3]}
            lookAt={[0, 0, 0]}
        />
        <AmbientLight {scene} intensity={1.25} />
        <DirectionalLight
            {scene}
            pos={[-1, 2, 0]}
            intensity={0.8}
            shadowMapSize={512 * 8}
            castShadow
        />

        <RandomGrid {scene} size={32} />

        <Mesh
            {scene}
            geometry={floorGeometry}
            material={floorMaterial}
            mat={{
                roughness: 0.5,
                metalness: 0.5,
                side: DoubleSide,
                color: 0xf7fafc,
            }}
            pos={[0, 0, 0]}
            rot={[MathUtils.degToRad(-90), 0, 0]}
            scale={[1, 1, 1]}
            receiveShadow
        />

        <OrbitControls {scene} enableDamping />
    </Scene>

    <WebGLRenderer
        {sti}
        sceneId="scene1"
        camId="cam1"
        config={{ antialias: true, alpha: true }}
        enableShadowMap
        shadowMapType={PCFSoftShadowMap}
    />
</Canvas>
