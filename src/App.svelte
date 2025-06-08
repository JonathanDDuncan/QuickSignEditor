<script lang="ts">
      console.log("App.svelte loaded");
  import SignEditor from './lib/SignEditor.svelte';
  import SymbolChooser from './lib/SymbolChooser.svelte';
  import Toolbar from './lib/Toolbar.svelte';

  // Example sign data with a couple of symbols
  let signData = {
    name: "Test Sign",
    symbols: [
      { id: 'S1001', x: 50, y: 60 },
      { id: 'S1002', x: 120, y: 100 }
    ]
  };

  // Example symbol library
  let symbols = [
    { id: 'S1001', name: 'Hand A', x: 0, y: 0 },
    { id: 'S1002', name: 'Hand B', x: 0, y: 0 },
    { id: 'S1003', name: 'Face Smile', x: 0, y: 0 }
  ];

  function handleSymbolSelect(symbol: { id: string; name: string }) {
    // Add symbol to signData at default position
    signData = {
      ...signData,
      symbols: [
        ...signData.symbols,
        { id: symbol.id, x: 80, y: 80 }
      ]
    };
  }

  function handleToolbarAction(action: string) {
    // Placeholder for toolbar actions
    alert(`Toolbar action: ${action}`);
  }
</script>

<main>
  <h1>QuickSignEditor Demo</h1>
  <Toolbar on:action={e => handleToolbarAction(e.detail)} />
  <div style="display: flex; gap: 2rem;">
    <SymbolChooser {symbols} on:select={e => handleSymbolSelect(e.detail)} />
    <SignEditor {signData} />
  </div>
</main>

<style>
  main {
    padding: 2rem;
    font-family: system-ui, sans-serif;
  }
</style>