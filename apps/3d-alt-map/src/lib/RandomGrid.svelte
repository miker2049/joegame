<script lang="ts">
 import  BoxGrid  from './BoxGrid.svelte'
 export let size: number
 export let scene


 let g: number[][] = []
 for(let y = 0; y < size; y++){
     g[y] = []
     for(let x = 0; x<size; x++){
         g[y][x] = Math.floor(Math.random()*5)
     }
 }

</script>

        <BoxGrid
            {scene}
            ux={0.5}
            uy={0.5}
            heights={g}
        />
