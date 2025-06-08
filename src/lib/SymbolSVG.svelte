<script lang="ts">
// Svelte version of Elm's symbolsvgmargincolor and symbolview
// Usage: <SymbolSVG symbol={symbol} scale={1} margin={0} className="" />

import { generateSymbolSVG, type SignWritingSymbol } from './signWritingFont';
import { onMount } from 'svelte';

export let symbol: SignWritingSymbol;
export let scale: number = 1;
export let margin: number = 0;
export let className: string = "";

// Fallbacks - SignWriting requires white fill and black line for proper dual-font rendering
$: fillColor = symbol.nwcolor || '#ffffff';  // SuttonSignWritingFill should be white
$: lineColor = symbol.nbcolor || '#000000';  // SuttonSignWritingLine should be black
$: symbolWidth = Math.round((symbol.width || 40) * (symbol.size || 1) * scale);
$: symbolHeight = Math.round((symbol.height || 40) * (symbol.size || 1) * scale);

let svgContent = '';
let mounted = false;
let loading = false;

onMount(() => {
  mounted = true;
  updateSVG();
});

$: if (mounted && symbol) {
  updateSVG();
}

async function updateSVG() {
  if (loading) return;
  
  try {
    loading = true;
    svgContent = await generateSymbolSVG(symbol, {
      size: Math.max(symbolWidth, symbolHeight),
      line: lineColor,
      fill: fillColor,
      scale: scale
    });
  } catch (error) {
    console.warn('Failed to generate SVG:', error);
    svgContent = `<svg width="${symbolWidth}" height="${symbolHeight}" viewBox="0 0 ${symbolWidth} ${symbolHeight}" xmlns="http://www.w3.org/2000/svg">
      <rect width="100%" height="100%" fill="none" stroke="#ccc" stroke-dasharray="2,2"/>
      <text x="50%" y="50%" text-anchor="middle" font-size="12" fill="#666">?</text>
    </svg>`;
  } finally {
    loading = false;
  }
}

</script>

{#if svgContent}
  <div
    class={className}
    style={`margin:${margin}px; width:${symbolWidth}px; height:${symbolHeight}px;`}
    role="img"
    aria-label={`SignWriting symbol ${symbol.key}`}
  >
    {@html svgContent}
  </div>
{:else}
  <svg
    class={className}
    width={symbolWidth}
    height={symbolHeight}
    viewBox={`0 0 ${symbolWidth} ${symbolHeight}`}
    style={`margin:${margin}px;`}
    xmlns="http://www.w3.org/2000/svg"
    role="img"
    aria-label={`SignWriting symbol ${symbol.key}`}
  >
    <rect width="100%" height="100%" fill="none" stroke="#ccc" stroke-dasharray="2,2"/>
    <text x="50%" y="50%" text-anchor="middle" font-size="12" fill="#666">Loading...</text>
  </svg>
{/if}

<style>
/* Component styles for symbol rendering */
div {
  display: inline-block;
}
</style>
