<script lang="ts">
  import { validateSignData } from './utils/validation';
  import type { SignData, Symbol as SignSymbol } from './utils/types';

  export let signData: SignData = { name: '', symbols: [] };

  let selectedSymbolId: string | null = null;
  let draggingId: string | null = null;
  let dragOffset = { x: 0, y: 0 };

  function selectSymbol(id: string) {
    selectedSymbolId = id;
  }

  function onSymbolMouseDown(event: MouseEvent, symbol: SignSymbol) {
    draggingId = symbol.id;
    dragOffset = {
      x: event.clientX - symbol.x,
      y: event.clientY - symbol.y
    };
    window.addEventListener('mousemove', onMouseMove);
    window.addEventListener('mouseup', onMouseUp);
  }

  function onMouseMove(event: MouseEvent) {
    if (!draggingId) return;
    const idx = signData.symbols.findIndex(s => s.id === draggingId);
    if (idx !== -1) {
      signData.symbols[idx].x = event.clientX - dragOffset.x;
      signData.symbols[idx].y = event.clientY - dragOffset.y;
    }
  }

  function onMouseUp() {
    draggingId = null;
    window.removeEventListener('mousemove', onMouseMove);
    window.removeEventListener('mouseup', onMouseUp);
  }

  $: validation = validateSignData(signData);
</script>

{#if !validation.isValid}
  <div class="error">{validation.error}</div>
{/if}

<!-- Canvas with rendered symbols -->
<div class="sign-canvas" role="region" aria-label="Sign canvas">
  {#if signData.symbols.length === 0}
    <p>SignEditor canvas coming soon...</p>
  {:else}
    {#each signData.symbols as symbol (symbol.id)}
      <div
        class="symbol-render {selectedSymbolId === symbol.id ? 'selected' : ''} {draggingId === symbol.id ? 'dragging' : ''}"
        style="left: {symbol.x}px; top: {symbol.y}px;"
        data-testid={`symbol-${symbol.id}`}
        tabIndex={0}
        aria-selected={selectedSymbolId === symbol.id}
        on:click={() => selectSymbol(symbol.id)}
        on:mousedown={(e) => onSymbolMouseDown(e, symbol)}
      >
        {symbol.code || symbol.id}
      </div>
    {/each}
  {/if}
</div>

<style>
.sign-canvas {
  width: 500px;
  height: 500px;
  border: 1px solid #ccc;
  margin: 1rem 0;
  background: #fff;
  display: flex;
  align-items: center;
  justify-content: center;
  position: relative;
}
.symbol-render {
  position: absolute;
  background: #f0f8ff;
  border: 1px solid #b0c4de;
  border-radius: 4px;
  padding: 2px 6px;
  font-family: monospace;
  font-size: 1rem;
  pointer-events: auto;
  user-select: none;
  transition: box-shadow 0.15s, border 0.15s;
  cursor: grab;
}
.symbol-render.selected {
  border: 2px solid #0074d9;
  box-shadow: 0 0 0 2px #b3e0ff;
  background: #e6f7ff;
  z-index: 1;
}
.symbol-render.dragging {
  opacity: 0.7;
  cursor: grabbing;
  z-index: 2;
}
.error {
  color: #b00;
  margin-bottom: 0.5rem;
}
</style>
