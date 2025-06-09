<script lang="ts">
  // New layout structure using MainLayout
  import MainLayout from './lib/layout/MainLayout.svelte';
  import { signStore, signModel, loadManiquinData } from './lib/signStore';
  import { onMount } from 'svelte';

  // Layout state
  let showAboutDialog = false;
  let showDrawer = false;
  let showKeyboard = false;

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
  // SignWriting font toggle removed - dual-font rendering is always used
  // Test function using the official SSW library (now working!)
  async function testOfficialSSW() {
    try {
      if (!window.ssw) {
        alert('SSW library not loaded!');
        return;
      }

      // Test FSW string from our successful test
      const testFSW = "AS20310S26b02S33100M521x547S33100482x483S20310506x500S26b02503x520";
      
      // Generate SVG using official library
      const svg = window.ssw.svg(testFSW);
      
      // Display it in the demo area
      const demoOutput = document.getElementById('fsw-demo-output');
      if (demoOutput) {
        demoOutput.innerHTML = `
          <div>
            <div style="font-size: 12px; color: #666; margin-bottom: 10px;">Generated with Official SSW Library:</div>
            ${svg}
            <div style="font-size: 10px; color: #999; margin-top: 10px;">
              FSW: ${testFSW.substring(0, 30)}...
            </div>
          </div>
        `;
      }
      
      console.log('Official SSW SVG generated:', svg);
      console.log('SVG contains dual-font classes:', svg.includes('sym-fill') && svg.includes('sym-line'));
        } catch (error) {
      console.error('Official SSW test failed:', error);
      alert('Official SSW test failed: ' + String(error));
    }
  }

  // Test additional FSW strings
  async function testMultipleFSWStrings() {
    if (!window.ssw) {
      alert('SSW library not loaded!');
      return;
    }

    const testStrings = [
      "AS20310S26b02S33100M521x547S33100482x483S20310506x500S26b02503x520",
      "M518x533S1870a489x515S18701482x490S20500508x496S2e734500x468",
      "S10000482x483",
      "S20000500x500"
    ];

    console.log('Testing multiple FSW strings...');
    testStrings.forEach((fsw, index) => {
      try {
        const svg = window.ssw.svg(fsw);
        const hasDualFont = svg.includes('sym-fill') && svg.includes('sym-line');
        console.log(`Test ${index + 1}: ${fsw}`);
        console.log(`  - SVG length: ${svg.length} chars`);
        console.log(`  - Has dual-font: ${hasDualFont}`);
        console.log(`  - SVG preview: ${svg.substring(0, 100)}...`);
      } catch (error) {
        console.error(`Error with FSW string ${index + 1}:`, error);
      }
    });
  }  async function testDualFontRendering() {
    try {
      if (!window.ssw) {
        alert('SSW library not loaded!');
        return;
      }
      
      const testSymbol = 'S10000';
      const svg = window.ssw.svg(testSymbol);
      
      console.log('Test SVG generated:', svg);
      
      // Create a visual test element
      const testDiv = document.createElement('div');
      testDiv.style.cssText = `
        position: fixed;
        top: 50px;
        left: 50px;
        background: white;
        border: 2px solid #333;
        padding: 15px;
        z-index: 10000;
        box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      `;
      
      testDiv.innerHTML = `
        <h4>Generated Official SSW SVG Test</h4>
        <div style="background: #f0f0f0; padding: 10px; margin: 10px 0;">
          ${svg}
        </div>
        <p style="font-size: 12px;">Above should show a symbol with white fill and black outline</p>
        <button onclick="this.parentElement.remove()">Close</button>
      `;
      
      document.body.appendChild(testDiv);
      
      return svg;    } catch (error) {
      console.error('Test failed:', error);
      alert('Test failed: ' + String(error));
    }
  }// Example model for demo/testing
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
  const unsubscribe = signModel.subscribe(value => maniquinModel = value);  onMount(() => {
    console.log('App onMount - calling loadManiquinData');
    loadManiquinData();
    
    // Debug: Check if data was loaded immediately
    setTimeout(() => {
      console.log('Checking mannequin data after load:', $signModel);
    }, 100);    // Make debugging functions available globally
    (window as any).debugMannequin = () => {
      console.log('=== MANNEQUIN DEBUG ===');
      console.log('Current mannequin model:', maniquinModel);
      console.log('Number of symbols:', maniquinModel.maniquinchoosings?.length || 0);
      if (maniquinModel.maniquinchoosings?.length > 0) {
        console.log('First few symbols:', maniquinModel.maniquinchoosings.slice(0, 5));
        console.log('Sample symbol structure:', maniquinModel.maniquinchoosings[0]);
      }
    };
    
    (window as any).reloadMannequinData = () => {
      console.log('=== RELOADING MANNEQUIN DATA ===');
      loadManiquinData();
      setTimeout(() => {
        console.log('After reload - symbols:', $signModel.maniquinchoosings?.length || 0);
      }, 100);
    };

    // Wait for SSW library to load and test
    console.log('Checking for SSW library...');
    const checkSSW = () => {
      if (window.ssw) {
        console.log('SSW library loaded successfully');
        console.log('Testing SSW functions:');
        console.log('- ssw.svg function available:', typeof window.ssw.svg === 'function');
        console.log('- ssw.size function available:', typeof window.ssw.size === 'function');
        console.log('- ssw.key function available:', typeof window.ssw.key === 'function');
        
        // Test basic functionality
        try {
          const testSize = window.ssw.size('S10000');
          console.log('- Test size for S10000:', testSize);
          
          const testSVG = window.ssw.svg('S10000');
          console.log('- Test SVG generation successful, length:', testSVG.length);
          console.log('- Contains dual-font classes:', testSVG.includes('sym-fill') && testSVG.includes('sym-line'));
        } catch (error) {
          console.warn('SSW function test failed:', error);
        }
      } else {
        console.log('SSW library not yet loaded, retrying...');
        setTimeout(checkSSW, 100);
      }
    };
    
    // Start checking after a brief delay
    setTimeout(checkSSW, 100);
    
    return () => unsubscribe();
  });// Example symbol for demo (uses proper SignWriting symbol key)
  const demoSymbol = {
    key: 'S100', // Index finger - confirmed valid SignWriting symbol key
    width: 40,
    height: 40,
    size: 1,
    nwcolor: '#ffffff', // fill color
    nbcolor: '#000000'  // line color
  };// Additional demo symbols to show variety
  const demoSymbols = [
    { key: 'S100', name: 'Index', color: '#ff6b6b' },
    { key: 'S101', name: 'Index on Circle', color: '#4ecdc4' },
    { key: 'S102', name: 'Index on Cup', color: '#45b7d1' },
    { key: 'S103', name: 'Test Symbol', color: '#96ceb4' }
  ];
</script>

<!-- Use MainLayout for proper layout hierarchy -->
<MainLayout
  {signData}
  {showAboutDialog}
  {showDrawer}
  {showKeyboard}
  on:toggleAbout={() => showAboutDialog = !showAboutDialog}
  on:toggleDrawer={() => showDrawer = !showDrawer}
  on:toggleKeyboard={() => showKeyboard = !showKeyboard}
  on:toolbarAction={e => handleToolbarAction(e.detail)}
  on:symbolSelect={e => handleSymbolSelect(e.detail)}
  on:symbolSelection={handleSymbolSelection}
  {chooserModel}
  {maniquinModel}
  {symbols}
/>

<!-- Development/Testing Section - Can be removed in production -->
<div style="position: fixed; bottom: 10px; right: 10px; background: rgba(0,0,0,0.8); color: white; padding: 10px; border-radius: 4px; font-size: 12px; z-index: 1000;">
  <div>Layout Hierarchy Active</div>
  <div>Keyboard: {showKeyboard ? 'On' : 'Off'}</div>
  <div>Drawer: {showDrawer ? 'On' : 'Off'}</div>
  <div>Symbols: {signData.symbols.length}</div>
</div>

<style>
  /* Global styles for the app */
  :global(body) {
    margin: 0;
    padding: 0;
    font-family: system-ui, -apple-system, sans-serif;
    background-color: #f5f5f5;
  }

  /* Development indicator styles */
  :global(.dev-indicator) {
    position: fixed;
    bottom: 10px;
    right: 10px;
    background: rgba(0,0,0,0.8);
    color: white;
    padding: 8px 12px;
    border-radius: 4px;
    font-size: 11px;
    font-family: monospace;
    z-index: 1000;
    pointer-events: none;
  }
</style>