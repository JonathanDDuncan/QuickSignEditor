<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import type { Symbol } from './utils/types';
  // Props
  export let symbols: Symbol[] = [];
  export let filter: string = '';

  const dispatch = createEventDispatcher();

  // Filtered and sorted symbols
  $: filteredSymbols = filter
    ? symbols.filter(s => s.id.includes(filter) || (s.code && s.code.includes(filter)))
    : symbols;
</script>

<div class="symbol-chooser">
  <input
    class="symbol-search"
    type="text"
    placeholder="Search symbols..."
    bind:value={filter}
    aria-label="Search symbols"
  />
  <div class="symbol-list" data-testid="symbol-list">
    {#each filteredSymbols as symbol (symbol.id)}
      <button
        class="symbol-item"
        type="button"
        on:click={() => dispatch('select', symbol)}
        aria-label={symbol.id}
        data-testid={`symbol-${symbol.id}`}
      >
        <!-- Render symbol SVG or image here -->
        <span>{symbol.id}</span>
      </button>
    {/each}
    {#if filteredSymbols.length === 0}
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
