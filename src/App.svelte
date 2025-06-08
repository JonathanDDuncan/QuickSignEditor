<script lang="ts">
  import SignEditor from './lib/SignEditor.svelte';
  import SymbolChooser from './lib/SymbolChooser.svelte';
  import Toolbar from './lib/Toolbar.svelte';
  import { signStore, signModel, loadManiquinData } from './lib/signStore';
  import { get, writable } from 'svelte/store';
  import ChooserPanel from './lib/ChooserPanel.svelte';
  import { onMount } from 'svelte';
  import Maniquin from './lib/Maniquin.svelte';

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
  function handleSymbolSelection(event: CustomEvent<{ id: string; multi: boolean }>) {
    const { id, multi } = event.detail;
    // Update signStore based on selection mode
    signStore.update(sd => ({
      ...sd,
      symbols: multi 
        ? sd.symbols.map(s => s.id === id ? { ...s, selected: !s.selected } : s)
        : sd.symbols.map(s => ({ ...s, selected: s.id === id }))
    }));
  }

  function handleToolbarAction(action: string) {
    if (action === 'undo') signStore.undo();
    else if (action === 'redo') signStore.redo();
    else alert(`Toolbar action: ${action}`);
  }

  // Example model for demo/testing
  let chooserModel = {
    groupselected: { symbolkey: 'S10000' },
    symbols: [
      { id: 'S10000', code: 'S10000', rowname: 'fist', category: 'Hand' },
      { id: 'S20000', code: 'S20000', rowname: 'circle', category: 'Hand' }
    ],
    filter: '',
    categories: ['Hand', 'Face'],
    selectedCategory: 'Hand',
    handGroupData: { label: 'Hand Group', items: [1, 2, 3] },
    generalGroupData: { label: 'General Group', items: [4, 5, 6] }
  };

  let maniquinModel: { maniquinchoosings: any[] } = { maniquinchoosings: [] };
  const unsubscribe = signModel.subscribe(value => maniquinModel = value);
  onMount(() => {
    loadManiquinData();
    return () => unsubscribe();
  });
</script>

<main>
  <h1>QuickSignEditor Demo</h1>
  <Toolbar on:action={e => handleToolbarAction(e.detail)} />
  <div class="editor-layout">
    <SymbolChooser {symbols} on:select={e => handleSymbolSelect(e.detail)} />
    <SignEditor {signData} on:select={handleSymbolSelection} />
  </div>
  <ChooserPanel model={chooserModel} parentWidth={800} parentHeight={400} />
  <!-- Add Maniquin preview using loaded data -->
  <Maniquin model={maniquinModel} width={400} height={80} />
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