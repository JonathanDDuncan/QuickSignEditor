<script lang="ts">
// Svelte version of Elm's symbolsvgmargincolor and symbolview
// Usage: <SymbolSVG symbol={symbol} scale={1} margin={0} className="" />

export let symbol: {
  key: string,
  width: number,
  height: number,
  size?: number,
  nwcolor?: string, // fill color
  nbcolor?: string, // line color
  unicodepua?: string // e.g. "U+FD830"
};
export let scale: number = 1;
export let margin: number = 0;
export let className: string = "";

// Fallbacks
$: fillColor = symbol.nwcolor || '#fff';
$: lineColor = symbol.nbcolor || '#000';
$: symbolWidth = Math.round((symbol.width || 40) * (symbol.size || 1) * scale);
$: symbolHeight = Math.round((symbol.height || 40) * (symbol.size || 1) * scale);

// Helper: convert U+FD830 to actual char
function puaCharCode(key: string): string {
  if (typeof key === 'string' && key.startsWith("U+")) {
    return String.fromCodePoint(parseInt(key.slice(2), 16));
  }
  return key || '';
}

// For demo: use unicodepua for both fill and line (real logic may differ)
$: fillChar = puaCharCode(symbol.unicodepua || symbol.key);
$: lineChar = puaCharCode(symbol.unicodepua || symbol.key);
// If you want to support both fill and line codepoints separately, you can add:
// $: fillChar = puaCharCode(symbol.fillunicodepua || symbol.unicodepua || symbol.key);
// $: lineChar = puaCharCode(symbol.lineunicodepua || symbol.unicodepua || symbol.key);
// and pass those fields in your symbol data.

</script>

<svg
  class={className}
  width={symbolWidth}
  height={symbolHeight}
  viewBox={`0 0 ${symbolWidth} ${symbolHeight}`}
  style={`margin:${margin}px;`}
  xmlns="http://www.w3.org/2000/svg"
>
  <g>
    <text
      class="sym-fill"
      x="0"
      y={symbolHeight * 0.8}
      font-family="SuttonSignWritingFill"
      font-size={symbolHeight}
      fill={fillColor}
      style="pointer-events:none;"
      aria-hidden="true"
    >{fillChar}</text>
    <text
      class="sym-line"
      x="0"
      y={symbolHeight * 0.8}
      font-family="SuttonSignWriting"
      font-size={symbolHeight}
      fill={lineColor}
      style="pointer-events:none;"
      aria-hidden="true"
    >{lineChar}</text>
  </g>
</svg>

<style>
.sym-fill {
  pointer-events: none;
}
.sym-line {
  pointer-events: none;
}
</style>
