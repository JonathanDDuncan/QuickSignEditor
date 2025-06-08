<!--
  @fileoverview Toolbar component with action controls
  Provides undo/redo, copy/paste, alignment, and other tools
-->
<script>
  import { createEventDispatcher } from 'svelte';
  import { signStore, historyStore, canUndoStore } from '../stores/signData.js';
  import { uiStore, hasSelection, hasClipboard, isMultiSelection } from '../stores/ui.js';
  import { alignSymbols } from '../utils/clipboard';

  const dispatch = createEventDispatcher();

  // Reactive statements
  $: canUndo = $canUndoStore;
  $: hasSelected = $hasSelection;
  $: hasClipboardData = $hasClipboard;
  $: isMultiSelect = $isMultiSelection;
  $: currentTool = $uiStore.currentTool;
  $: zoom = $uiStore.zoom;

  /**
   * Handle undo action
   */
  function handleUndo() {
    if (!canUndo) return;

    const previousState = historyStore.undo();
    if (previousState) {
      signStore.setSignData(previousState);
      uiStore.clearSelection();
    }
  }

  /**
   * Handle copy action
   */
  function handleCopy() {
    if (!hasSelected) return;

    const symbols = $signStore.symbols;
    const selectedSymbols = $uiStore.selectedSymbols;
    const symbolsToCopy = symbols.filter(s => selectedSymbols.includes(s.id));
    
    uiStore.setClipboard(symbolsToCopy);
    dispatch('action', { type: 'copy', count: symbolsToCopy.length });
  }

  /**
   * Handle paste action
   */
  function handlePaste() {
    if (!hasClipboardData) return;

    const clipboardData = $uiStore.clipboard;
    // Add pasted symbols at a slight offset
    const newSymbols = clipboardData.map((symbol, index) => ({
      ...symbol,
      id: `symbol_${Date.now()}_${index}`,
      x: symbol.x + 20,
      y: symbol.y + 20,
      selected: true
    }));

    const currentSymbols = $signStore.symbols;
    signStore.setSymbols([...currentSymbols, ...newSymbols]);
    
    // Select the new symbols
    uiStore.setSelectedSymbols(newSymbols.map(s => s.id));
    
    dispatch('action', { type: 'paste', count: newSymbols.length });
  }

  /**
   * Handle delete action
   */
  function handleDelete() {
    if (!hasSelected) return;

    const symbols = $signStore.symbols;
    const selectedSymbols = $uiStore.selectedSymbols;
    const remainingSymbols = symbols.filter(s => !selectedSymbols.includes(s.id));
    
    // Save state for undo
    historyStore.addState($signStore, 'delete symbols');
    
    signStore.setSymbols(remainingSymbols);
    uiStore.clearSelection();
    
    dispatch('action', { type: 'delete', count: selectedSymbols.length });
  }

  /**
   * Handle alignment action
   * @param {string} alignment - Alignment type
   */
  function handleAlign(alignment) {
    if (!isMultiSelect) return;

    const symbols = $signStore.symbols;
    const alignedSymbols = alignSymbols(symbols, alignment);
    
    // Save state for undo
    historyStore.addState($signStore, `align ${alignment}`);
    
    signStore.setSymbols(alignedSymbols);
    dispatch('action', { type: 'align', alignment });
  }

  /**
   * Handle tool selection
   * @param {string} tool - Tool name
   */
  function handleToolSelect(tool) {
    uiStore.setCurrentTool(tool);
    dispatch('toolChange', { tool });
  }

  /**
   * Handle zoom change
   * @param {number} delta - Zoom delta (-1 for zoom out, 1 for zoom in)
   */
  function handleZoom(delta) {
    const newZoom = zoom + (delta * 0.1);
    uiStore.setZoom(newZoom);
    dispatch('zoomChange', { zoom: newZoom });
  }

  /**
   * Reset zoom to 100%
   */
  function handleZoomReset() {
    uiStore.setZoom(1.0);
    dispatch('zoomChange', { zoom: 1.0 });
  }

  /**
   * Toggle grid visibility
   */
  function handleToggleGrid() {
    uiStore.toggleGrid();
    dispatch('toggleGrid');
  }

  /**
   * Open symbol chooser
   */
  function handleOpenSymbolChooser() {
    uiStore.toggleSymbolChooser();
    dispatch('openSymbolChooser');
  }

  /**
   * Handle FSW export
   */
  function handleExportFSW() {
    dispatch('exportFSW');
  }

  /**
   * Handle FSW import
   */
  function handleImportFSW() {
    dispatch('importFSW');
  }
</script>

<div class="toolbar" role="toolbar" aria-label="SignWriting tools">
  <!-- File operations -->
  <div class="toolbar-group">
    <button 
      class="toolbar-button"
      on:click={handleImportFSW}
      title="Import FSW"
      aria-label="Import FSW file"
    >
      📁
    </button>
    <button 
      class="toolbar-button"
      on:click={handleExportFSW}
      title="Export FSW"
      aria-label="Export as FSW"
    >
      💾
    </button>
  </div>

  <div class="toolbar-separator"></div>

  <!-- Edit operations -->
  <div class="toolbar-group">
    <button 
      class="toolbar-button"
      class:disabled={!canUndo}
      on:click={handleUndo}
      disabled={!canUndo}
      title="Undo (Ctrl+Z)"
      aria-label="Undo last action"
    >
      ↶
    </button>
    <button 
      class="toolbar-button"
      class:disabled={!hasSelected}
      on:click={handleCopy}
      disabled={!hasSelected}
      title="Copy (Ctrl+C)"
      aria-label="Copy selected symbols"
    >
      📋
    </button>
    <button 
      class="toolbar-button"
      class:disabled={!hasClipboardData}
      on:click={handlePaste}
      disabled={!hasClipboardData}
      title="Paste (Ctrl+V)"
      aria-label="Paste symbols"
    >
      📄
    </button>
    <button 
      class="toolbar-button delete-button"
      class:disabled={!hasSelected}
      on:click={handleDelete}
      disabled={!hasSelected}
      title="Delete (Del)"
      aria-label="Delete selected symbols"
    >
      🗑️
    </button>
  </div>

  <div class="toolbar-separator"></div>

  <!-- Alignment tools -->
  <div class="toolbar-group">
    <button 
      class="toolbar-button"
      class:disabled={!isMultiSelect}
      on:click={() => handleAlign('horizontal')}
      disabled={!isMultiSelect}
      title="Align horizontally"
      aria-label="Align symbols horizontally"
    >
      ⊟
    </button>
    <button 
      class="toolbar-button"
      class:disabled={!isMultiSelect}
      on:click={() => handleAlign('vertical')}
      disabled={!isMultiSelect}
      title="Align vertically"
      aria-label="Align symbols vertically"
    >
      ⊞
    </button>
    <button 
      class="toolbar-button"
      class:disabled={!isMultiSelect}
      on:click={() => handleAlign('center')}
      disabled={!isMultiSelect}
      title="Center align"
      aria-label="Center align symbols"
    >
      ⊡
    </button>
  </div>

  <div class="toolbar-separator"></div>

  <!-- Tools -->
  <div class="toolbar-group">
    <button 
      class="toolbar-button tool-button"
      class:active={currentTool === 'select'}
      on:click={() => handleToolSelect('select')}
      title="Select tool"
      aria-label="Select tool"
      aria-pressed={currentTool === 'select'}
    >
      ▶
    </button>
    <button 
      class="toolbar-button tool-button"
      class:active={currentTool === 'move'}
      on:click={() => handleToolSelect('move')}
      title="Move tool"
      aria-label="Move tool"
      aria-pressed={currentTool === 'move'}
    >
      ✋
    </button>
  </div>

  <div class="toolbar-separator"></div>

  <!-- View controls -->
  <div class="toolbar-group">
    <button 
      class="toolbar-button"
      on:click={() => handleZoom(-1)}
      title="Zoom out"
      aria-label="Zoom out"
    >
      🔍-
    </button>
    <span class="zoom-display">{Math.round(zoom * 100)}%</span>
    <button 
      class="toolbar-button"
      on:click={() => handleZoom(1)}
      title="Zoom in"
      aria-label="Zoom in"
    >
      🔍+
    </button>
    <button 
      class="toolbar-button"
      on:click={handleZoomReset}
      title="Reset zoom (100%)"
      aria-label="Reset zoom to 100%"
    >
      🎯
    </button>
  </div>

  <div class="toolbar-separator"></div>

  <!-- Symbol chooser -->
  <div class="toolbar-group">
    <button 
      class="toolbar-button primary-button"
      on:click={handleOpenSymbolChooser}
      title="Add symbol"
      aria-label="Open symbol chooser"
    >
      ➕ Symbol
    </button>
  </div>

  <div class="toolbar-separator"></div>

  <!-- Grid toggle -->
  <div class="toolbar-group">
    <button 
      class="toolbar-button"
      class:active={$uiStore.showGrid}
      on:click={handleToggleGrid}
      title="Toggle grid"
      aria-label="Toggle grid visibility"
      aria-pressed={$uiStore.showGrid}
    >
      #️⃣
    </button>
  </div>
</div>

<style>
  .toolbar {
    display: flex;
    align-items: center;
    gap: 8px;
    padding: 8px 12px;
    background: linear-gradient(to bottom, #f8f8f8, #e8e8e8);
    border: 1px solid #ccc;
    border-radius: 4px;
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
    flex-wrap: wrap;
  }

  .toolbar-group {
    display: flex;
    align-items: center;
    gap: 4px;
  }

  .toolbar-button {
    background: linear-gradient(to bottom, #fff, #f0f0f0);
    border: 1px solid #ccc;
    border-radius: 3px;
    padding: 6px 10px;
    cursor: pointer;
    font-size: 0.9rem;
    transition: all 0.2s;
    min-width: 32px;
    height: 32px;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .toolbar-button:hover:not(.disabled) {
    background: linear-gradient(to bottom, #f0f8ff, #e0f0ff);
    border-color: #0066cc;
  }

  .toolbar-button:active:not(.disabled) {
    background: linear-gradient(to bottom, #e0f0ff, #d0e8ff);
    transform: translateY(1px);
  }

  .toolbar-button.active {
    background: linear-gradient(to bottom, #0066cc, #0052a3);
    color: white;
    border-color: #0052a3;
  }

  .toolbar-button.disabled {
    background: #f5f5f5;
    color: #999;
    cursor: not-allowed;
    border-color: #ddd;
  }

  .toolbar-button.primary-button {
    background: linear-gradient(to bottom, #0066cc, #0052a3);
    color: white;
    border-color: #0052a3;
    font-weight: 500;
    padding: 6px 12px;
  }

  .toolbar-button.primary-button:hover {
    background: linear-gradient(to bottom, #0052a3, #004080);
  }

  .toolbar-button.delete-button:hover:not(.disabled) {
    background: linear-gradient(to bottom, #ffe6e6, #ffcccc);
    border-color: #cc0000;
    color: #cc0000;
  }

  .toolbar-separator {
    width: 1px;
    height: 24px;
    background: #ccc;
    margin: 0 4px;
  }

  .zoom-display {
    font-size: 0.8rem;
    color: #666;
    font-weight: 500;
    min-width: 40px;
    text-align: center;
  }

  /* Responsive design */
  @media (max-width: 768px) {
    .toolbar {
      padding: 6px 8px;
    }

    .toolbar-button {
      min-width: 28px;
      height: 28px;
      padding: 4px 6px;
      font-size: 0.8rem;
    }

    .zoom-display {
      font-size: 0.7rem;
      min-width: 35px;
    }
  }

  /* Focus styles for accessibility */
  .toolbar-button:focus {
    outline: 2px solid #0066cc;
    outline-offset: 2px;
  }

  /* High contrast mode support */
  @media (prefers-contrast: high) {
    .toolbar {
      border-color: #000;
    }

    .toolbar-button {
      border-color: #000;
    }

    .toolbar-separator {
      background: #000;
    }
  }
</style>
