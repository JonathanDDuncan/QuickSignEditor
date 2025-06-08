<script lang="ts">
  import { createEventDispatcher, onMount } from 'svelte';
  import type { Symbol } from './utils/types';
  // Props
  export let symbols: Symbol[] = [];
  export let filter: string = '';

  // Add category support
  export let categories: string[] = [];
  export let selectedCategory: string = '';

  const dispatch = createEventDispatcher();

  // Focus the search input on mount for accessibility and legacy parity
  let searchInput: HTMLInputElement | null = null;
  onMount(() => {
    if (searchInput) searchInput.focus();
  });

  // Filtered and sorted symbols with category
  $: filteredSymbols = symbols.filter(s => {
    const matchesCategory = !selectedCategory || (s.category === selectedCategory);
    const matchesFilter = !filter || s.id.includes(filter) || (s.code && s.code.includes(filter));
    return matchesCategory && matchesFilter;
  });

  // --- Legacy parity: Keyboard navigation for symbol chooser list ---
  let focusedIndex = -1;
  $: visibleSymbols = filteredSymbols;

  function focusSymbol(idx: number) {
    focusedIndex = idx;
    // Focus the button in the DOM
    setTimeout(() => {
      const btns = document.querySelectorAll('.symbol-list .symbol-item');
      if (btns[idx]) (btns[idx] as HTMLElement).focus();
    }, 0);
  }

  function onSearchKeyDown(e: KeyboardEvent) {
    // ArrowDown: focus first/next symbol
    if (e.key === 'ArrowDown') {
      if (visibleSymbols.length > 0) {
        focusSymbol(0);
        e.preventDefault();
      }
    }
    // ArrowUp: focus last symbol
    if (e.key === 'ArrowUp') {
      if (visibleSymbols.length > 0) {
        focusSymbol(visibleSymbols.length - 1);
        e.preventDefault();
      }
    }
    // Enter key: if only one result, select it (legacy behavior)
    if (e.key === 'Enter' && visibleSymbols.length === 1) {
      dispatch('select', visibleSymbols[0]);
      e.preventDefault();
    }
    // Escape key: clear search
    if (e.key === 'Escape') {
      filter = '';
      e.preventDefault();
    }
  }

  function onSymbolKeyDown(e: KeyboardEvent, idx: number) {
    if (e.key === 'ArrowDown') {
      if (idx < visibleSymbols.length - 1) focusSymbol(idx + 1);
      e.preventDefault();
    }
    if (e.key === 'ArrowUp') {
      if (idx > 0) focusSymbol(idx - 1);
      else searchInput?.focus();
      e.preventDefault();
    }
    if (e.key === 'Enter' || e.key === ' ') {
      dispatch('select', visibleSymbols[idx]);
      e.preventDefault();
    }
  }

  function onCategoryChange(event: Event) {
    const value = (event.target as HTMLSelectElement).value;
    selectedCategory = value;
    // Optionally, dispatch event for parent to react
    dispatch('category', value);
  }

  // --- Legacy parity: Layout and visual grouping ---
  // Group symbols by row/col if present (legacy chooser style)
  $: groupedSymbols = (() => {
    // If symbols have row/col, group by row (legacy: rowname/colname)
    const groups: Record<string, Symbol[]> = {};
    for (const s of visibleSymbols) {
      const row = (s as any).rowname || 'Other';
      if (!groups[row]) groups[row] = [];
      groups[row].push(s);
    }
    return groups;
  })();
  $: groupOrder = Object.keys(groupedSymbols);
</script>

<div class="symbol-chooser">
  <select
    class="symbol-category"
    bind:value={selectedCategory}
    on:change={onCategoryChange}
    aria-label="Filter by category"
  >
    <option value="">All Categories</option>
    {#each categories as cat}
      <option value={cat}>{cat}</option>
    {/each}
  </select>
  <input
    class="symbol-search"
    type="text"
    placeholder="Search symbols..."
    bind:value={filter}
    aria-label="Search symbols"
    bind:this={searchInput}
    autocomplete="off"
    spellcheck="false"
    autocapitalize="off"
    on:keydown={onSearchKeyDown}
  />
  <div class="symbol-list" data-testid="symbol-list">
    {#if groupOrder.length > 1}
      {#each groupOrder as row}
        <div class="symbol-group">
          <div class="symbol-group-label">{row}</div>
          <div class="symbol-group-row">
            {#each groupedSymbols[row] as symbol, idx (symbol.id)}
              <button
                class="symbol-item"
                type="button"
                on:click={() => dispatch('select', symbol)}
                aria-label={symbol.id}
                data-testid={`symbol-${symbol.id}`}
                tabindex="0"
                on:keydown={(e) => onSymbolKeyDown(e, idx)}
              >
                <span>{symbol.id}</span>
              </button>
            {/each}
          </div>
        </div>
      {/each}
    {:else}
      {#each visibleSymbols as symbol, idx (symbol.id)}
        <button
          class="symbol-item"
          type="button"
          on:click={() => dispatch('select', symbol)}
          aria-label={symbol.id}
          data-testid={`symbol-${symbol.id}`}
          tabindex="0"
          on:keydown={(e) => onSymbolKeyDown(e, idx)}
        >
          <span>{symbol.id}</span>
        </button>
      {/each}
    {/if}
    {#if visibleSymbols.length === 0}
      <div class="no-results">No symbols found.</div>
    {/if}
  </div>
</div>

<style>
.symbol-chooser {
  display: flex;
  flex-direction: column;
  width: 260px;
  height: 100%;
  border-right: 1px solid #ccc;
  background: #fafbfc;
  min-width: 0;
}
.symbol-category {
  margin: 8px 8px 0 8px;
  padding: 6px 10px;
  border: 1px solid #bbb;
  border-radius: 4px;
  font-size: 15px;
  background: #fff;
}
.symbol-search {
  margin: 8px;
  padding: 6px 10px;
  border: 1px solid #bbb;
  border-radius: 4px;
  font-size: 15px;
}
.symbol-list {
  flex: 1;
  overflow-y: auto;
  padding: 8px;
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
}
.symbol-item {
  display: flex;
  align-items: center;
  justify-content: center;
  min-width: 48px;
  min-height: 48px;
  background: #fff;
  border: 1px solid #bbb;
  border-radius: 4px;
  font-size: 18px;
  cursor: pointer;
  transition: box-shadow 0.1s, border-color 0.1s;
  box-sizing: border-box;
  padding: 0.5em 0.7em;
}
.symbol-item:focus, .symbol-item:hover {
  border-color: #0078d4;
  box-shadow: 0 0 0 2px #0078d4;
  outline: none;
}
.no-results {
  color: #888;
  margin: 24px auto;
  text-align: center;
  width: 100%;
}
.symbol-group {
  margin-bottom: 12px;
}
.symbol-group-label {
  font-weight: bold;
  font-size: 1em;
  margin: 4px 0 2px 4px;
  color: #444;
}
.symbol-group-row {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
}
@media (max-width: 800px) {
  .symbol-chooser {
    width: 100% !important;
    min-width: 0;
    border-right: none;
    border-bottom: 1px solid #ccc;
    padding-bottom: 8px;
  }
  .symbol-list {
    gap: 10px;
  }
  .symbol-item {
    min-width: 44px;
    min-height: 44px;
    font-size: 16px;
    padding: 0.5em 0.5em;
  }
}
@media (max-width: 500px) {
  .symbol-chooser {
    width: 100% !important;
    padding: 0;
  }
  .symbol-item {
    min-width: 38px;
    min-height: 38px;
    font-size: 15px;
    padding: 0.4em 0.4em;
  }
}
</style>
