<script lang="ts">
 import {Mesh, MeshStandardMaterial, BoxBufferGeometry} from 'svelthree'


 export let scene: any
 export let x: number
 export let y: number
 export let height: number

  let cubeMaterial = new MeshStandardMaterial();
  let cubeGeometry = new BoxBufferGeometry(1, height, 1);
</script>

    <Mesh
      {scene}
      geometry={cubeGeometry}
      material={cubeMaterial}
      mat={{ roughness: 0.5, metalness: 0.5, color: 0xff3e00 }}
      pos={[x, height/2, y]}
      rot={[0, 0, 0]}
      castShadow
      receiveShadow />
