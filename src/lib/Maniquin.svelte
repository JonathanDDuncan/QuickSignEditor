<script lang="ts">
// Svelte version of legacy Elm Maniquin
import { onMount } from 'svelte';
import { generateFSWSVGSync } from './signWritingFont';
export let model: any;
// Remove width/height constraints to allow natural sizing
// export let width: number = 200;
// export let height: number = 80;

// Calculate scale to fit content in given width/height (keep for legacy individual symbols)
function calculatescale(contentWidth: number, contentHeight: number, maxWidth: number, maxHeight: number) {
  if (contentWidth === 0 || contentHeight === 0) return 1;
  return Math.min(maxWidth / contentWidth, maxHeight / contentHeight);
}

// Compute bounding box for all choosings
function getchoosingssize(choosings: any[]) {
  return choosings.reduce(
    (acc, c) => {
      const right = Math.max(acc.right, c.offset.offsetx + c.displaySign.width);
      const left = Math.min(acc.left, c.offset.offsetx);
      const bottom = Math.max(acc.bottom, c.offset.offsety + c.displaySign.height);
      const top = Math.min(acc.top, c.offset.offsety);
      return { top, bottom, right, left };
    },
    { top: 0, bottom: 0, right: 0, left: 0 }
  );
}

// Compute content size
function getchoosingsview(choosings: any[], bottompadding: number) {
  const size = getchoosingssize(choosings);
  const width = size.right - size.left;
  const height = size.bottom - size.top + bottompadding;
  return { width, height, left: size.left, top: size.top };
}

// Svelte: we render SVG for each choosing (stub for now)

// --- FIX: move scale calculation to a reactive statement ---
let scale = 1;
let viewSize = { width: 0, height: 0, left: 0, top: 0 };
// Only calculate scale for individual symbols, not complete FSW
// $: scale = calculatescale(viewSize.width, viewSize.height, width, height);

// --- REACTIVE STATE FOR VIEW AND SCALE ---
$: if (model && model.maniquinchoosings && model.maniquinchoosings.length > 0) {
  console.log('Mannequin: Data loaded with', model.maniquinchoosings.length, 'symbols');
  console.log('Mannequin: Sample symbol:', model.maniquinchoosings[0]);
  console.log('Mannequin: SSW library available:', typeof window !== 'undefined' && window.ssw);
  
  // Only calculate size and scale for individual symbols, not complete FSW
  if (!model.maniquinchoosings[0].displaySign?.completeFSW) {
    viewSize = getchoosingsview(model.maniquinchoosings, 10);
    scale = calculatescale(viewSize.width, viewSize.height, 800, 600); // Use larger default dimensions
  }
} else {
  console.log('Mannequin: No data or empty data');
  viewSize = { width: 0, height: 0, left: 0, top: 0 };
  scale = 1;
}

// Event handlers following the Elm implementation patterns
function handleSymbolClick(choosing: any) {
  // Equivalent to Elm's onClick ((EditorMsg << Editor.Clicked) choosing.value)
  console.log('Symbol clicked:', choosing.value);
  // TODO: Dispatch to editor for handling
}

function handleSymbolDrag(choosing: any) {
  // Equivalent to Elm's onMouseDown ((EditorMsg << Editor.DragSymbol) (firstsymbol choosing).key)
  console.log('Symbol drag started:', choosing);
  // TODO: Implement drag functionality
}

function handleSymbolDoubleClick(choosing: any) {
  // Equivalent to Elm's onDoubleClick ((EditorMsg << Editor.ReplaceSymbol) (firstsymbol choosing).key)
  console.log('Symbol double-clicked:', choosing);
  // TODO: Implement replace symbol functionality
}

function firstsymbol(choosing: any) {
  // Helper function to get the first symbol like in Elm implementation
  return choosing.displaySign?.syms?.[0] || { key: '' };
}
</script>

{#if model && model.maniquinchoosings && model.maniquinchoosings.length > 0}
  {#key model.maniquinchoosings}    {#if model.maniquinchoosings[0].displaySign?.completeFSW}
      <!-- Handle complete FSW - let the SSW library determine the natural size -->
      <div class="maniquin-complete-fsw">
        <div style="padding: 10px;">
          <h4>Complete FSW Mannequin:</h4>
          <div class="fsw-container">
            {@html generateFSWSVGSync(model.maniquinchoosings[0].displaySign.fsw, { scale: 1.0, fill: '#ffffff', line: '#000000' })}
          </div>
        </div>
      </div>
    {:else}
      <!-- Handle individual symbols with positioning -->
      {#if viewSize.width > 0 && viewSize.height > 0}        <div
          class="maniquin-content"
          style="transform: scale({scale}); transform-origin: top left; height: {viewSize.height * scale}px; width: 800px;"
        >
            <svg
              width={viewSize.width}
              height={viewSize.height}
              style="position: relative; display: block; border: 2px solid red; background: yellow;"
            >
              <!-- Offset all content by -left, -top so bounding box starts at 0,0 -->
              <g transform="translate({-viewSize.left}, {-viewSize.top})">
                {#each model.maniquinchoosings as choosing}
                  <g
                    transform="translate({choosing.offset.offsetx}, {choosing.offset.offsety})"
                    role="button"
                    tabindex="0"
                    aria-label="SignWriting symbol {choosing.displaySign.label || 'Unknown'}"
                    on:click={() => handleSymbolClick(choosing)}
                    on:mousedown={() => handleSymbolDrag(choosing)}
                    on:dblclick={() => handleSymbolDoubleClick(choosing)}
                    on:keydown={(e) => e.key === 'Enter' && handleSymbolClick(choosing)}
                    style="cursor: pointer;"
                  >
                    {#if choosing.displaySign && choosing.displaySign.fsw}
                      <!-- Embed the SVG content directly, stripping the outer <svg> tag -->
                      {@html generateFSWSVGSync(choosing.displaySign.fsw, { scale: 1, fill: '#ffffff', line: '#000000' }).replace(/<svg[^>]*>/, '').replace(/<\/svg>/, '')}
                    {:else if choosing.displaySign && choosing.displaySign.svg}
                      {@html choosing.displaySign.svg}
                    {:else}
                      <rect
                        x="0" y="0"
                        width={choosing.displaySign.width}
                        height={choosing.displaySign.height}
                        fill="#e6f0fa"
                        stroke="#0078d4"
                        stroke-width="1"
                        rx="4"
                      />
                      <text x="4" y="16" font-size="12" fill="#0078d4">{choosing.displaySign.label || 'Sign'}</text>
                    {/if}
                  </g>
                {/each}
              </g>
            </svg>
          </div>
        {:else}
          <div class="placeholder">No maniquin content.</div>
        {/if}
    {/if}
  {/key}
{:else}
  <div class="placeholder">No maniquin content.</div>
{/if}

<style>
.maniquin-content {
  width: 100%;
  height: 100%;
  background: #f0f0f0;
  border-bottom: 1px solid #ccc;
  overflow: hidden;
}

.maniquin-complete-fsw {
  width: 100%;
  height: auto;
  background: #f8f8f8;
  border: 1px solid #ddd;
  border-radius: 4px;
  overflow: auto; /* Allow scrolling if content is too large */
}

.fsw-container {
  display: flex;
  justify-content: center;
  align-items: flex-start;
  min-height: 300px;
  padding: 20px;
}

.fsw-container :global(svg) {
  max-width: 100%;
  height: auto;
  border: 1px solid #ccc;
  background: white;
  border-radius: 4px;
}

.placeholder {
  color: #888;
  font-size: 1em;
  text-align: center;
  padding: 16px 0;
}
</style>
