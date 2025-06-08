<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import { signStore } from './signStore';
  import { validateSignData } from './utils/validation';
  import transformSymbols from './utils/transform';
  import type { SignData, Symbol } from './utils/types';

  export let signData: SignData | undefined;
  let validation: { isValid: boolean; error?: string } = { isValid: true };
  const dispatch = createEventDispatcher();

  // Always derive symbols from signData or store
  $: currentSignData = signData ? signData : $signStore;
  $: symbols = transformSymbols(currentSignData.symbols);
  $: validation = validateSignData(currentSignData);
  $: showPlaceholder = symbols.length === 0 && validation.isValid;

  function selectSymbol(id: string) {
    if (signData) {
      // Prop-driven: dispatch event for parent to handle selection
      dispatch('select', id);
    } else {
      // Store-driven: update selection in the store
      signStore.update(sd => ({
        ...sd,
        symbols: sd.symbols.map(s => ({ ...s, selected: s.id === id }))
      }));
    }
  }

  let draggingId: string | null = null;
  let dragOffset = { x: 0, y: 0 };
  let startPos = { x: 0, y: 0 };

  function onSymbolMouseDown(event: MouseEvent, symbol: Symbol) {
    draggingId = symbol.id;
    startPos = { x: symbol.x, y: symbol.y };
    dragOffset = {
      x: event.clientX - symbol.x,
      y: event.clientY - symbol.y
    };
    window.addEventListener('mousemove', onMouseMove);
    window.addEventListener('mouseup', onMouseUp);
  }

  function onMouseMove(event: MouseEvent) {
    if (!draggingId) return;
    const newX = event.clientX - dragOffset.x;
    const newY = event.clientY - dragOffset.y;
    if (signData) {
      // Prop-driven: dispatch event for parent to handle position update
      dispatch('move', { id: draggingId, x: newX, y: newY });
    } else {
      // Store-driven: update position in the store
      signStore.update(sd => ({
        ...sd,
        symbols: sd.symbols.map(s =>
          s.id === draggingId ? { ...s, x: newX, y: newY } : s
        )
      }));
    }
  }

  function onMouseUp() {
    draggingId = null;
    window.removeEventListener('mousemove', onMouseMove);
    window.removeEventListener('mouseup', onMouseUp);
  }
</script>

<div class="sign-editor-canvas" aria-label="Sign canvas">
  {#if showPlaceholder}
    <div class="placeholder">SignEditor canvas coming soon...</div>
  {/if}
  {#each symbols as symbol (symbol.id)}
    <button
      type="button"
      class="symbol{symbol.selected ? ' selected' : ''}"
      data-testid={`canvas-symbol-${symbol.id}`}
      style="left: {symbol.x}px; top: {symbol.y}px; transform: rotate({symbol.rotation || 0}deg);"
      aria-selected={symbol.selected}
      role="option"
      on:click|stopPropagation={() => selectSymbol(symbol.id)}
      on:mousedown|stopPropagation={(e) => onSymbolMouseDown(e, symbol)}
      draggable="false"
    >
      <!-- Render symbol SVG or image here -->
      <span>{symbol.id}</span>
    </button>
  {/each}
  {#if !validation.isValid}
    <div class="error">{validation.error}</div>
  {/if}
</div>

<style>
.sign-editor-canvas {
  position: relative;
  width: 100%;
  height: 400px;
  background: #f8f8f8;
  border: 1px solid #ccc;
  overflow: auto;
  outline: none;
  min-width: 0;
}
.symbol {
  position: absolute;
  cursor: pointer;
  user-select: none;
  padding: 8px 10px;
  background: #fff;
  border: 1px solid #bbb;
  border-radius: 3px;
  font-size: 16px;
  transition: box-shadow 0.1s;
  min-width: 44px;
  min-height: 44px;
  box-sizing: border-box;
}
.symbol[aria-selected="true"], .symbol.selected {
  box-shadow: 0 0 0 2px #0078d4;
  background: #e6f0fa;
}
.symbol:focus {
  outline: 2px solid #0078d4;
  z-index: 1;
}
@media (max-width: 800px) {
  .sign-editor-canvas {
    height: 300px;
  }
  .symbol {
    font-size: 15px;
    padding: 8px 8px;
    min-width: 40px;
    min-height: 40px;
  }
}
@media (max-width: 500px) {
  .sign-editor-canvas {
    height: 220px;
  }
  .symbol {
    font-size: 14px;
    padding: 6px 6px;
    min-width: 36px;
    min-height: 36px;
  }
}
.error {
  position: absolute;
  bottom: 8px;
  left: 8px;
  color: #b00;
  background: #fff0f0;
  padding: 4px 8px;
  border-radius: 3px;
  font-size: 13px;
  border: 1px solid #fbb;
}
.placeholder {
  color: #888;
  text-align: center;
  margin-top: 160px;
  font-size: 1.2em;
}
</style>
