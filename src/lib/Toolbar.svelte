<script lang="ts">
  import { createEventDispatcher } from 'svelte';

  // Provide default no-op functions for all handler props
  export let onUndo: () => void = () => {};
  export let onRedo: () => void = () => {};
  export let onCopy: () => void = () => {};
  export let onPaste: () => void = () => {};
  export let onAlign: () => void = () => {};
  export let onImportFSW: () => void = () => {};
  export let onExportFSW: () => void = () => {};

  const dispatch = createEventDispatcher();

  function handle(action: string) {
    dispatch(action);
    switch (action) {
      case 'undo': onUndo && onUndo(); break;
      case 'redo': onRedo && onRedo(); break;
      case 'copy': onCopy && onCopy(); break;
      case 'paste': onPaste && onPaste(); break;
      case 'align': onAlign && onAlign(); break;
      case 'importFSW': onImportFSW && onImportFSW(); break;
      case 'exportFSW': onExportFSW && onExportFSW(); break;
    }
  }
</script>

<div class="toolbar" role="toolbar" aria-label="Editor toolbar">
  <button type="button" aria-label="Undo" on:click={() => handle('undo')} data-testid="undo-btn">⎌ Undo</button>
  <button type="button" aria-label="Redo" on:click={() => handle('redo')} data-testid="redo-btn">↻ Redo</button>
  <button type="button" aria-label="Copy" on:click={() => handle('copy')} data-testid="copy-btn">⧉ Copy</button>
  <button type="button" aria-label="Paste" on:click={() => handle('paste')} data-testid="paste-btn">⎘ Paste</button>
  <button type="button" aria-label="Align" on:click={() => handle('align')} data-testid="align-btn">☰ Align</button>
  <button type="button" aria-label="Import FSW" on:click={() => handle('importFSW')} data-testid="import-fsw-btn">⇪ Import</button>
  <button type="button" aria-label="Export FSW" on:click={() => handle('exportFSW')} data-testid="export-fsw-btn">⇩ Export</button>
</div>

<style>
.toolbar {
  display: flex;
  gap: 0.5rem;
  background: #f5f5f5;
  border-bottom: 1px solid #ccc;
  padding: 0.5rem 0.75rem;
  align-items: center;
  flex-wrap: wrap;
}
.toolbar button {
  background: #fff;
  border: 1px solid #bbb;
  border-radius: 3px;
  padding: 0.5rem 1rem;
  font-size: 1rem;
  cursor: pointer;
  transition: background 0.2s;
  min-width: 44px;
  min-height: 44px;
  box-sizing: border-box;
}
.toolbar button:hover, .toolbar button:focus {
  background: #e0e0e0;
  outline: 2px solid #0078d4;
}
@media (max-width: 800px) {
  .toolbar {
    flex-direction: column;
    align-items: stretch;
    gap: 0.25rem;
    padding: 0.5rem 0.25rem;
  }
  .toolbar button {
    width: 100%;
    font-size: 0.98rem;
    padding: 0.7rem 0.5rem;
    min-width: 40px;
    min-height: 40px;
  }
}
@media (max-width: 500px) {
  .toolbar {
    padding: 0.25rem 0.1rem;
  }
  .toolbar button {
    font-size: 0.95rem;
    padding: 0.6rem 0.3rem;
    min-width: 36px;
    min-height: 36px;
  }
}
</style>
