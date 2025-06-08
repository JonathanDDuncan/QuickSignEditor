<script lang="ts">
      console.log("App.svelte loaded");
  import SignEditor from './lib/SignEditor.svelte';
  import SymbolChooser from './lib/SymbolChooser.svelte';
  import Toolbar from './lib/Toolbar.svelte';
  import { signStore } from './lib/signStore';
  import { get } from 'svelte/store';

  // Example symbol library
  let symbols = [
    { id: 'S1001', name: 'Hand A', x: 0, y: 0 },
    { id: 'S1002', name: 'Hand B', x: 0, y: 0 },
    { id: 'S1003', name: 'Face Smile', x: 0, y: 0 }
  ];

  // Use signStore for signData
  $: signData = $signStore.present;

  function handleSymbolSelect(symbol: { id: string; name: string; [key: string]: any }) {
    // Add symbol to signStore at default position, with all properties from the chooser symbol
    const newSymbol = { ...symbol, code: symbol.id, x: 80, y: 80, selected: false };
    signStore.update(sd => ({
      ...sd,
      symbols: [...sd.symbols.filter(s => s.id !== newSymbol.id), newSymbol],
      // Force new object reference for history
      name: sd.name ? (sd.name + '') : '',
      metadata: { ...(sd.metadata || {}) }
    }));
  }

  function handleSymbolSelection(id: string) {
    // Update signStore so only the selected symbol is marked selected
    signStore.update(sd => ({
      ...sd,
      symbols: sd.symbols.map(s => ({ ...s, selected: s.id === id }))
    }));
  }

  function handleToolbarAction(action: string) {
    if (action === 'undo') signStore.undo();
    else if (action === 'redo') signStore.redo();
    else alert(`Toolbar action: ${action}`);
  }
</script>

<main>
  <h1>QuickSignEditor Demo</h1>
  <Toolbar on:action={e => handleToolbarAction(e.detail)} />
  <div class="editor-layout">
    <SymbolChooser {symbols} on:select={e => handleSymbolSelect(e.detail)} />
    <SignEditor {signData} on:select={e => handleSymbolSelection(e.detail)} />
  </div>
</main>

<style>
  main {
    padding: 2rem;
    font-family: system-ui, sans-serif;
  }
  .editor-layout {
    display: flex;
    gap: 2rem;
  }
  @media (max-width: 800px) {
    main {
      padding: 1rem;
    }
    .editor-layout {
      flex-direction: column;
      gap: 1rem;
    }
    .symbol-chooser,
    .sign-editor-canvas {
      width: 100% !important;
      min-width: 0;
      max-width: 100vw;
    }
  }
  @media (max-width: 500px) {
    main {
      padding: 0.5rem;
      font-size: 15px;
    }
    h1 {
      font-size: 1.2em;
    }
  }
</style>