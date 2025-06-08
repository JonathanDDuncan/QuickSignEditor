<script lang="ts">
  import type { Symbol } from './utils/types';
  import { onMount } from 'svelte';

  export let symbols: Symbol[];
  export let filter: string = '';
  export let onSelect: (symbol: Symbol) => void = () => {};

  let filteredSymbols: Symbol[] = [];

  $: filteredSymbols = filter
    ? symbols.filter(s =>
        (s.code || '').toLowerCase().includes(filter.toLowerCase())
      )
    : symbols;

  function handleSelect(symbol: Symbol) {
    onSelect(symbol);
  }
</script>

<div class="symbol-chooser">
  <input
    type="text"
    placeholder="Search symbols..."
    bind:value={filter}
    aria-label="Search symbols"
    class="symbol-search"
  />
  <div class="symbol-list" data-testid="symbol-list">
    {#if filteredSymbols.length === 0}
      <div class="no-results">No symbols found.</div>
    {:else}
      {#each filteredSymbols as symbol (symbol.id)}
        <button
          class="symbol-item"
          type="button"
          on:click={() => handleSelect(symbol)}
          data-testid={`symbol-${symbol.id}`}
        >
          {symbol.code || symbol.id}
        </button>
      {/each}
    {/if}
  </div>
</div>

<style>
.symbol-chooser {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  width: 300px;
}
.symbol-search {
  padding: 0.5rem;
  font-size: 1rem;
  border: 1px solid #ccc;
  border-radius: 4px;
}
.symbol-list {
  display: flex;
  flex-wrap: wrap;
  gap: 0.25rem;
  min-height: 2rem;
}
.symbol-item {
  padding: 0.25rem 0.5rem;
  border: 1px solid #aaa;
  background: #f9f9f9;
  border-radius: 3px;
  cursor: pointer;
  font-family: monospace;
}
.symbol-item:hover {
  background: #e0e0e0;
}
.no-results {
  color: #888;
  font-size: 0.95rem;
}
</style>
