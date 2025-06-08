<script lang="ts">
      console.log("App.svelte loaded");
  import SignEditor from './lib/SignEditor.svelte';
  import SymbolChooser from './lib/SymbolChooser.svelte';
  import Toolbar from './lib/Toolbar.svelte';

  // Example sign data with a couple of symbols
  let signData = {
    name: "Test Sign",
    symbols: [
      { id: 'S1001', x: 50, y: 60, selected: false },
      { id: 'S1002', x: 120, y: 100, selected: false }
    ]
  };

  // Example symbol library
  let symbols = [
    { id: 'S1001', name: 'Hand A', x: 0, y: 0 },
    { id: 'S1002', name: 'Hand B', x: 0, y: 0 },
    { id: 'S1003', name: 'Face Smile', x: 0, y: 0 }
  ];

  function handleSymbolSelect(symbol: { id: string; name: string; [key: string]: any }) {
    // Add symbol to signData at default position, with all properties from the chooser symbol
    const newSymbol = { ...symbol, code: symbol.id, x: 80, y: 80, selected: false };
    signData = {
      ...signData,
      symbols: [...signData.symbols.filter(s => s.id !== newSymbol.id), newSymbol]
    };
    signData = { ...signData, symbols: [...signData.symbols] }; // Force Svelte reactivity
    console.log('signData.symbols after add:', signData.symbols);
  }

  function handleSymbolSelection(id: string) {
    // Update signData so only the selected symbol is marked selected
    signData = {
      ...signData,
      symbols: signData.symbols.map(s => ({ ...s, selected: s.id === id }))
    };
    signData = { ...signData };
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
    <SignEditor signData={signData} on:select={e => handleSymbolSelection(e.detail)} />
  </div>
</main>

<style>
  main {
    padding: 2rem;
    font-family: system-ui, sans-serif;
  }
</style>