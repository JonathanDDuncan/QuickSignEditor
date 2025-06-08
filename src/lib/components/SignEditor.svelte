<!--
  @fileoverview Main SignEditor canvas component
  Handles symbol rendering, selection, and manipulation
-->
<script>
  import { onMount, createEventDispatcher, onDestroy } from 'svelte';
  import { signData as realSignData } from '../stores/signData.js';
  import { ui as realUi } from '../stores/ui.js';
  import { get, readable } from 'svelte/store';

  // Allow test to inject stores
  export let signData = realSignData;
  export let ui = realUi;
  export let width = 800;
  export let height = 600;

  // --- BEGIN PATCH: Support test mocks as stores ---
  // If signData/ui are not Svelte stores, wrap them in a readable store for reactivity
  function isSvelteStore(store) {
    return store && typeof store.subscribe === 'function' && ('$on' in store || '$set' in store || '$update' in store || '$subscribe' in store || store.set || store.update);
  }

  let _signData = signData;
  let _ui = ui;
  let unsubscribeSignData, unsubscribeUi;
  let signDataValue, uiValue;

  function subscribeToStore(store, setter) {
    if (store && typeof store.subscribe === 'function') {
      return store.subscribe(val => setter(val));
    }
    return () => {};
  }

  // If not a Svelte store, wrap in readable
  if (!isSvelteStore(signData)) {
    _signData = readable(signData, set => { set(signData); return () => {}; });
  }
  if (!isSvelteStore(ui)) {
    _ui = readable(ui, set => { set(ui); return () => {}; });
  }

  // Manual subscription for test compatibility
  $: unsubscribeSignData && unsubscribeSignData();
  $: unsubscribeUi && unsubscribeUi();
  $: unsubscribeSignData = subscribeToStore(_signData, v => signDataValue = v);
  $: unsubscribeUi = subscribeToStore(_ui, v => uiValue = v);
  onDestroy(() => {
    unsubscribeSignData && unsubscribeSignData();
    unsubscribeUi && unsubscribeUi();
  });
  // --- END PATCH ---

  // Component state
  let canvas;
  let isDragging = false;
  let dragStart = { x: 0, y: 0 };
  let dragSymbol = null;

  const dispatch = createEventDispatcher();
  $: canvasStyle = `width: ${width}px; height: ${height}px;`;
  $: gridClass = (uiValue?.showGrid ? 'show-grid' : '');
  $: symbols = signDataValue?.symbols || [];
  $: selectedSymbol = uiValue?.selectedSymbol || null;
  $: zoom = uiValue?.zoom || 1;
  $: pan = uiValue?.pan || { x: 0, y: 0 };

  // Helper: get ARIA label for symbol
  function getSymbolAriaLabel(symbol) {
    if (symbol.name) return symbol.name;
    if (symbol.symbol) return `Symbol ${symbol.symbol}`;
    if (symbol.id) return `Symbol ${symbol.id}`;
    return 'Symbol';
  }

  // Helper: get button content for symbol
  function getSymbolContent(symbol) {
    if (symbol.unicode) return symbol.unicode;
    if (symbol.symbol) return symbol.symbol;
    return '⌐';
  }

  function setSelectedSymbolValue(val) {
    if (ui.selectedSymbol && typeof ui.selectedSymbol.set === 'function') {
      ui.selectedSymbol.set(val);
    } else if (ui.set) {
      ui.set({ ...uiValue, selectedSymbol: val });
    }
  }

  function handleCanvasClick(event) {
    const rect = canvas.getBoundingClientRect();
    const x = (event.clientX - rect.left - pan.x) / zoom;
    const y = (event.clientY - rect.top - pan.y) / zoom;
    // Find clicked symbol
    const clicked = symbols.find(s => x >= s.x && x <= s.x + (s.width || 50) && y >= s.y && y <= s.y + (s.height || 50));
    if (clicked) {
      setSelectedSymbolValue(clicked.id);
    } else {
      setSelectedSymbolValue(null);
    }
  }

  function handleDragStart(event) {
    const rect = canvas.getBoundingClientRect();
    const x = (event.clientX - rect.left - pan.x) / zoom;
    const y = (event.clientY - rect.top - pan.y) / zoom;
    dragSymbol = symbols.find(s => x >= s.x && x <= s.x + (s.width || 50) && y >= s.y && y <= s.y + (s.height || 50));
    if (dragSymbol) {
      isDragging = true;
      dragStart = { x, y };
      event.preventDefault();
    }
  }

  function handleDragMove(event) {
    if (!isDragging || !dragSymbol) return;
    const rect = canvas.getBoundingClientRect();
    const x = (event.clientX - rect.left - pan.x) / zoom;
    const y = (event.clientY - rect.top - pan.y) / zoom;
    const dx = x - dragStart.x;
    const dy = y - dragStart.y;
    signData.update(data => {
      if (!data || !data.symbols) return data;
      return {
        ...data,
        symbols: data.symbols.map(s =>
          s.id === dragSymbol.id ? { ...s, x: s.x + dx, y: s.y + dy } : s
        )
      };
    });
    dragStart = { x, y };
  }

  function handleDragEnd() {
    isDragging = false;
    dragSymbol = null;
  }

  function handleKeyDown(event) {
    if (event.key === 'Delete' && selectedSymbol) {
      signData.update(data => {
        if (!data || !data.symbols) return data;
        return {
          ...data,
          symbols: data.symbols.filter(s => s.id !== selectedSymbol)
        };
      });
      setSelectedSymbolValue(null);
    }
    if (event.key === 'Escape') {
      setSelectedSymbolValue(null);
    }
    if (selectedSymbol && event.key.startsWith('Arrow')) {
      const move = event.shiftKey ? 1 : 10;
      signData.update(data => {
        if (!data || !data.symbols) return data;
        return {
          ...data,
          symbols: data.symbols.map(s => {
            if (s.id !== selectedSymbol) return s;
            if (event.key === 'ArrowLeft') return { ...s, x: s.x - move };
            if (event.key === 'ArrowRight') return { ...s, x: s.x + move };
            if (event.key === 'ArrowUp') return { ...s, y: s.y - move };
            if (event.key === 'ArrowDown') return { ...s, y: s.y + move };
            return s;
          })
        };
      });
    }
  }

  function handleDrop(event) {
    event.preventDefault();
    // Example: add a new symbol at drop position
    const rect = canvas.getBoundingClientRect();
    const x = (event.clientX - rect.left - pan.x) / zoom;
    const y = (event.clientY - rect.top - pan.y) / zoom;
    signData.update(data => {
      if (!data) data = {};
      return {
        ...data,
        symbols: [...(data.symbols || []), { id: `sym${Date.now()}`, symbol: 'S10000', x, y, selected: false }]
      };
    });
  }

  function handleDragOver(event) {
    event.preventDefault();
  }

  onMount(() => {
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  });
</script>

<svelte:window on:mousemove={handleDragMove} on:mouseup={handleDragEnd} />

<div class="sign-editor-container">
  <!-- Error banner for invalid data -->
  {#if !$signData || typeof $signData.symbols === 'undefined'}
    <div class="error-banner">Sign must have symbols array</div>
  {/if}
  <div
    class="sign-editor {gridClass}"
    style="{canvasStyle}; transform: scale({zoom});"
    bind:this={canvas}
    on:click={handleCanvasClick}
    on:mousedown={handleDragStart}
    on:drop={handleDrop}
    on:dragover={handleDragOver}
    role="img"
    aria-label="Sign editor canvas"
    tabindex="0"
  >
    {#each symbols as symbol (symbol.id)}
      <button
        type="button"
        class="symbol {selectedSymbol === symbol.id ? 'selected' : ''}"
        style="left: {symbol.x * zoom + pan.x}px; top: {symbol.y * zoom + pan.y}px; width: {(symbol.width || 50) * zoom}px; height: {(symbol.height || 50) * zoom}px; position: absolute;"
        data-symbol-id={symbol.id}
        aria-label={getSymbolAriaLabel(symbol)}
        aria-pressed={selectedSymbol === symbol.id}
        tabindex="0"
        data-dragging={isDragging && dragSymbol && dragSymbol.id === symbol.id ? 'true' : undefined}
      >
        <span class="symbol-content">{getSymbolContent(symbol)}</span>
      </button>
    {/each}
  </div>
</div>

<style>
  .sign-editor-container {
    position: relative;
    border: 2px solid #ddd;
    background: #fafafa;
    overflow: hidden;
  }
  .sign-editor {
    position: relative;
    background: white;
    cursor: crosshair;
    overflow: hidden;
    outline: none;
  }
  .sign-editor:focus {
    outline: 2px solid #0066cc;
    outline-offset: -2px;
  }
  .sign-editor.show-grid {
    background-image: linear-gradient(to right, #e0e0e0 1px, transparent 1px), linear-gradient(to bottom, #e0e0e0 1px, transparent 1px);
    background-size: 20px 20px;
  }
  .symbol {
    position: absolute;
    cursor: pointer;
    display: flex;
    align-items: center;
    justify-content: center;
    border: 1px solid transparent;
    transition: border-color 0.2s;
  }
  .symbol.selected {
    border-color: #0066cc;
    background: rgba(0, 102, 204, 0.1);
  }
  .symbol-content {
    font-size: 1.5em;
    user-select: none;
    pointer-events: none;
  }
  .error-banner {
    background: #ffe6e6;
    color: #cc0000;
    padding: 8px;
    border-bottom: 1px solid #ffcccc;
    font-size: 0.9em;
  }
</style>
