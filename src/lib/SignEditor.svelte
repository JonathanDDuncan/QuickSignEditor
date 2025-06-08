<script lang="ts">
  import { createEventDispatcher, onMount } from 'svelte';
  import { signStore } from './signStore';
  import { validateSignData } from './utils/validation';
  import transformSymbols from './utils/transform';
  import type { SignData, Symbol } from './utils/types';

  export let signData: SignData | undefined;
  let validation: { isValid: boolean; error?: string } = { isValid: true };
  let unsubscribe: (() => void) | undefined;
  const dispatch = createEventDispatcher();

  // Always derive symbols from signData or store
  $: currentSignData = signData ? signData : $signStore;
  $: symbols = transformSymbols(currentSignData.symbols);
  $: validation = validateSignData(currentSignData);
  $: showPlaceholder = symbols.length === 0 && validation.isValid;

  onMount(() => {
    return () => { if (unsubscribe) unsubscribe(); };
  });

  function handleCanvasClick(event: MouseEvent) {
    // Placeholder for symbol selection logic
  }

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
</script>

<div class="sign-editor-canvas" tabindex="0" on:click={handleCanvasClick} aria-label="Sign canvas">
  {#if showPlaceholder}
    <div class="placeholder">SignEditor canvas coming soon...</div>
  {/if}
  {#each symbols as symbol (symbol.id)}
    <div
      class="symbol{symbol.selected ? ' selected' : ''}"
      data-testid={`canvas-symbol-${symbol.id}`}
      style="left: {symbol.x}px; top: {symbol.y}px; transform: rotate({symbol.rotation || 0}deg);"
      aria-selected={symbol.selected}
      tabindex="0"
      on:click|stopPropagation={() => selectSymbol(symbol.id)}
    >
      <!-- Render symbol SVG or image here -->
      <span>{symbol.id}</span>
    </div>
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
}
.symbol {
  position: absolute;
  cursor: pointer;
  user-select: none;
  padding: 2px 4px;
  background: #fff;
  border: 1px solid #bbb;
  border-radius: 3px;
  font-size: 14px;
  transition: box-shadow 0.1s;
}
.symbol[aria-selected="true"] {
  box-shadow: 0 0 0 2px #0078d4;
  background: #e6f0fa;
}
.symbol.selected {
  box-shadow: 0 0 0 2px #0078d4;
  background: #e6f0fa;
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
