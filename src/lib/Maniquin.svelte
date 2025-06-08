<script lang="ts">
// Svelte version of legacy Elm Maniquin
import { onMount } from 'svelte';
export let model: any;
export let width: number = 200;
export let height: number = 80;

// Calculate scale to fit content in given width/height
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
$: scale = calculatescale(viewSize.width, viewSize.height, width, height);

// --- REACTIVE STATE FOR VIEW AND SCALE ---
$: if (model && model.maniquinchoosings && model.maniquinchoosings.length > 0) {
  viewSize = getchoosingsview(model.maniquinchoosings, 10);
  scale = calculatescale(viewSize.width, viewSize.height, width, height);
} else {
  viewSize = { width: 0, height: 0, left: 0, top: 0 };
  scale = 1;
}
</script>

{#if model && model.maniquinchoosings && model.maniquinchoosings.length > 0}
  {#key model.maniquinchoosings}
    {#await Promise.resolve(getchoosingsview(model.maniquinchoosings, 10)) then vs}
      {() => { viewSize = vs; return null; }}
      {#if viewSize.width > 0 && viewSize.height > 0}
        <div
          class="maniquin-content"
          style="transform: scale({scale}); transform-origin: top left; height: {viewSize.height * scale}px; width: {width}px;"
        >
          <svg
            width={viewSize.width}
            height={viewSize.height}
            style="position: relative; display: block;"
          >
            <!-- Offset all content by -left, -top so bounding box starts at 0,0 -->
            <g transform="translate({-viewSize.left}, {-viewSize.top})">
              {#each model.maniquinchoosings as choosing}
                <g
                  transform="translate({choosing.offset.offsetx}, {choosing.offset.offsety})"
                  aria-label="Choosing"
                >
                  {#if choosing.displaySign && choosing.displaySign.svg}
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
    {/await}
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
.placeholder {
  color: #888;
  font-size: 1em;
  text-align: center;
  padding: 16px 0;
}
</style>
