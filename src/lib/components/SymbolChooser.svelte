<!--
  @fileoverview Symbol chooser component for browsing and selecting symbols
  Integrates with pure functions for symbol management
-->
<script>
  import { createEventDispatcher } from 'svelte';
  import { uiStore } from '../stores/ui.js';
  import { createSymbol, filterSymbolsByCategory } from '../utils/symbols';
  import { generateId } from '../utils/calculations';

  // Props
  export let isOpen = false;
  export let categories = [
    { id: 'hands', name: 'Hands', symbols: 50 },
    { id: 'movement', name: 'Movement', symbols: 30 },
    { id: 'contact', name: 'Contact', symbols: 25 },
    { id: 'expression', name: 'Expression', symbols: 20 }
  ];

  // Component state
  let selectedCategory = 'hands';
  let searchQuery = '';
  let filteredSymbols = [];

  const dispatch = createEventDispatcher();

  // Sample symbols for demonstration
  const sampleSymbols = {
    hands: [
      { id: 'h001', unicode: '👋', name: 'Open Hand', category: 'hands' },
      { id: 'h002', unicode: '✊', name: 'Fist', category: 'hands' },
      { id: 'h003', unicode: '👍', name: 'Thumbs Up', category: 'hands' },
      { id: 'h004', unicode: '☝️', name: 'Index Finger', category: 'hands' }
    ],
    movement: [
      { id: 'm001', unicode: '↑', name: 'Up Arrow', category: 'movement' },
      { id: 'm002', unicode: '↓', name: 'Down Arrow', category: 'movement' },
      { id: 'm003', unicode: '←', name: 'Left Arrow', category: 'movement' },
      { id: 'm004', unicode: '→', name: 'Right Arrow', category: 'movement' }
    ],
    contact: [
      { id: 'c001', unicode: '•', name: 'Touch Point', category: 'contact' },
      { id: 'c002', unicode: '◦', name: 'Contact', category: 'contact' },
      { id: 'c003', unicode: '⬢', name: 'Brush', category: 'contact' }
    ],
    expression: [
      { id: 'e001', unicode: '😊', name: 'Happy', category: 'expression' },
      { id: 'e002', unicode: '😢', name: 'Sad', category: 'expression' },
      { id: 'e003', unicode: '😮', name: 'Surprised', category: 'expression' }
    ]
  };

  // Reactive statements
  $: if (selectedCategory || searchQuery) {
    updateFilteredSymbols();
  }

  /**
   * Update filtered symbols based on category and search
   */
  function updateFilteredSymbols() {
    let symbols = sampleSymbols[selectedCategory] || [];
    
    if (searchQuery) {
      symbols = symbols.filter(symbol =>
        symbol.name.toLowerCase().includes(searchQuery.toLowerCase()) ||
        symbol.unicode.includes(searchQuery)
      );
    }
    
    filteredSymbols = symbols;
  }

  /**
   * Handle category selection
   * @param {string} categoryId - Selected category ID
   */
  function handleCategorySelect(categoryId) {
    selectedCategory = categoryId;
    searchQuery = '';
  }

  /**
   * Handle symbol selection
   * @param {Object} symbol - Selected symbol
   */
  function handleSymbolSelect(symbol) {
    const newSymbol = createSymbol(
      generateId(),
      symbol.unicode,
      100, // Default X position
      100, // Default Y position
      {
        name: symbol.name,
        category: symbol.category,
        rotation: 0,
        fill: 1,
        width: 50,
        height: 50
      }
    );

    dispatch('symbolSelected', { symbol: newSymbol });
  }

  /**
   * Handle search input
   * @param {Event} event - Input event
   */
  function handleSearch(event) {
    searchQuery = event.target.value;
  }

  /**
   * Close symbol chooser
   */
  function handleClose() {
    dispatch('close');
  }

  /**
   * Handle keyboard navigation
   * @param {Event} event - Keyboard event
   */
  function handleKeyDown(event) {
    if (event.key === 'Escape') {
      handleClose();
    }
  }
</script>

<svelte:window on:keydown={handleKeyDown} />

{#if isOpen}
  <div class="symbol-chooser-overlay" on:click={handleClose}>
    <div class="symbol-chooser" on:click|stopPropagation role="dialog" aria-label="Symbol chooser">
      <!-- Header -->
      <div class="chooser-header">
        <h2 id="symbol-chooser-title">Choose Symbol</h2>
        <button 
          class="close-button" 
          on:click={handleClose}
          aria-label="Close symbol chooser"
        >
          ✕
        </button>
      </div>

      <!-- Search -->
      <div class="search-section">
        <input
          type="search"
          placeholder="Search symbols..."
          value={searchQuery}
          on:input={handleSearch}
          class="search-input"
          aria-label="Search symbols"
          role="searchbox"
        />
      </div>

      <!-- Categories -->
      <div class="categories-section">
        <div class="category-tabs" role="tablist" aria-label="Symbol categories">
          {#each categories as category}
            <button
              class="category-tab"
              class:active={selectedCategory === category.id}
              on:click={() => handleCategorySelect(category.id)}
              aria-pressed={selectedCategory === category.id}
              role="tab"
              aria-selected={selectedCategory === category.id}
              aria-controls="symbols-panel-{category.id}"
              id="tab-{category.id}"
            >
              {category.name}
              <span class="symbol-count">({category.symbols})</span>
            </button>
          {/each}
        </div>
      </div>

      <!-- Symbols grid -->
      <div class="symbols-section" role="region" aria-label="Symbol chooser" aria-labelledby="symbol-chooser-title">
        {#if filteredSymbols.length === 0}
          <div class="no-symbols">
            {searchQuery ? 'No symbols match your search' : 'No symbols in this category'}
          </div>
        {:else}
          <div class="symbols-grid" data-testid="symbol-grid">
            {#each filteredSymbols as symbol}
              <button
                class="symbol-item"
                on:click={() => handleSymbolSelect(symbol)}
                title={symbol.name}
                aria-label="Drag to canvas: {symbol.name}"
                role="button"
                data-testid="symbol-visual"
                draggable="true"
                on:dragstart={(e) => e.dataTransfer.setData('text/plain', symbol.id)}
              >
                <span class="symbol-icon">{symbol.unicode}</span>
                <span class="symbol-name">{symbol.name}</span>
              </button>
            {/each}
          </div>
        {/if}
      </div>

      <!-- Footer info -->
      <div class="chooser-footer">
        <small>Click a symbol to add it to your sign</small>
      </div>
    </div>
  </div>
{/if}

<style>
  .symbol-chooser-overlay {
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: rgba(0, 0, 0, 0.5);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 1000;
  }

  .symbol-chooser {
    background: white;
    border-radius: 8px;
    box-shadow: 0 4px 20px rgba(0, 0, 0, 0.15);
    width: 90%;
    max-width: 600px;
    max-height: 80vh;
    display: flex;
    flex-direction: column;
  }

  .chooser-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 16px 20px;
    border-bottom: 1px solid #eee;
  }

  .chooser-header h2 {
    margin: 0;
    font-size: 1.25rem;
    color: #333;
  }

  .close-button {
    background: none;
    border: none;
    font-size: 1.5rem;
    cursor: pointer;
    padding: 4px;
    border-radius: 4px;
    color: #666;
  }

  .close-button:hover {
    background: #f5f5f5;
    color: #333;
  }

  .search-section {
    padding: 16px 20px;
    border-bottom: 1px solid #eee;
  }

  .search-input {
    width: 100%;
    padding: 8px 12px;
    border: 1px solid #ddd;
    border-radius: 4px;
    font-size: 1rem;
  }

  .search-input:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 2px rgba(0, 102, 204, 0.2);
  }

  .categories-section {
    padding: 16px 20px 0;
  }

  .category-tabs {
    display: flex;
    gap: 8px;
    flex-wrap: wrap;
  }

  .category-tab {
    background: #f5f5f5;
    border: 1px solid #ddd;
    border-radius: 20px;
    padding: 8px 16px;
    cursor: pointer;
    font-size: 0.9rem;
    transition: all 0.2s;
  }

  .category-tab:hover {
    background: #e8e8e8;
  }

  .category-tab.active {
    background: #0066cc;
    color: white;
    border-color: #0066cc;
  }

  .symbol-count {
    font-size: 0.8rem;
    opacity: 0.7;
  }

  .symbols-section {
    flex: 1;
    padding: 20px;
    overflow-y: auto;
  }

  .symbols-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(100px, 1fr));
    gap: 12px;
  }

  .symbol-item {
    background: white;
    border: 1px solid #ddd;
    border-radius: 8px;
    padding: 12px;
    cursor: pointer;
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 8px;
    transition: all 0.2s;
    min-height: 80px;
  }

  .symbol-item:hover {
    border-color: #0066cc;
    box-shadow: 0 2px 8px rgba(0, 102, 204, 0.15);
    transform: translateY(-1px);
  }

  .symbol-icon {
    font-size: 1.5rem;
    line-height: 1;
  }

  .symbol-name {
    font-size: 0.75rem;
    text-align: center;
    color: #666;
    line-height: 1.2;
  }

  .no-symbols {
    text-align: center;
    color: #999;
    padding: 40px;
    font-style: italic;
  }

  .chooser-footer {
    padding: 12px 20px;
    border-top: 1px solid #eee;
    background: #f9f9f9;
    text-align: center;
    color: #666;
  }

  /* Responsive design */
  @media (max-width: 480px) {
    .symbol-chooser {
      width: 95%;
      height: 90vh;
    }

    .symbols-grid {
      grid-template-columns: repeat(auto-fill, minmax(80px, 1fr));
      gap: 8px;
    }

    .symbol-item {
      min-height: 70px;
      padding: 8px;
    }
  }
</style>
