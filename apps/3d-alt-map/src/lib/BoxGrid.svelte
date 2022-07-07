<script lang="ts">
    import Box from "./Box.svelte";
    export let scene;
    export let heights: number[][];
    let offsetX = heights[0].length / 2;
    let offsetY = heights.length / 2;
    export let ux = 0;
    export let uy = 0;
</script>

<div>
    {#each heights as row, y}
        {#each row as item, x}
            <Box
                {scene}
                x={x - offsetX + ux}
                y={y - offsetY + uy}
                height={item}
            />
        {/each}
    {/each}
</div>
