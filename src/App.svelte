<script lang="ts">  import SignEditor from './lib/SignEditor.svelte';
  import SymbolChooser from './lib/SymbolChooser.svelte';
  import Toolbar from './lib/Toolbar.svelte';
  import { signStore, signModel, loadManiquinData } from './lib/signStore';
  import { get, writable } from 'svelte/store';
  import ChooserPanel from './lib/ChooserPanel.svelte';  import { onMount } from 'svelte';
  import Maniquin from './lib/Maniquin.svelte';
  import SymbolSVG from './lib/SymbolSVG.svelte';
  import { loadSignWritingFonts, generateSymbolSVG } from './lib/signWritingFont';
  // SignWriting uses dual-font rendering (fill + line together) - no toggle needed

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
      alert('Official SSW test failed: ' + error.message);
    }
  }

  // Test function to demonstrate dual-font rendering
  async function testDualFontRendering() {
    try {
      const testSymbol = {
        key: 'S10000',
        width: 40,
        height: 40,
        size: 1
      };
      
      const svg = await generateSymbolSVG(testSymbol, { size: 60 });
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
        <h4>Generated Dual-Font SVG Test</h4>
        <div style="background: #f0f0f0; padding: 10px; margin: 10px 0;">
          ${svg}
        </div>
        <p style="font-size: 12px;">Above should show a symbol with white fill and black outline</p>
        <button onclick="this.parentElement.remove()">Close</button>
      `;
      
      document.body.appendChild(testDiv);
      
      return svg;
    } catch (error) {
      console.error('Test failed:', error);
      alert('Test failed: ' + error.message);
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
    loadManiquinData();

    // Load SignWriting fonts and test library
    console.log('Testing SuttonSignWriting.js library...');
    loadSignWritingFonts().then(() => {
      console.log('SignWriting fonts loaded successfully');// Test the library directly with various calls
      if (window.ssw) {
        console.log('Testing direct ssw calls:');
        console.log('- ssw.sizes object:', window.ssw.sizes);
        console.log('- Object.keys(ssw.sizes):', Object.keys(window.ssw.sizes));
        console.log('- ssw.size("S10000"):', window.ssw.size('S10000'));
        console.log('- ssw.key("S10000"):', window.ssw.key('S10000'));
          // Test if we can manually add the size
        (window.ssw.sizes as any)['S10000'] = '76,76';
        console.log('- After manual size addition, ssw.size("S10000"):', window.ssw.size('S10000'));
        console.log('- After manual size addition, ssw.svg("S10000"):', window.ssw.svg('S10000'));
      }
    }).catch(error => {
      console.warn('Failed to load SignWriting fonts:', error);
    });
    return () => unsubscribe();
  });  // Example symbol for demo (uses proper SignWriting symbol key)
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

<main>
  <h1>QuickSignEditor Demo</h1>
  <!-- Demo: SuttonSignWriting font rendering example -->  <section style="margin-bottom: 2rem;">
    <h2>SignWriting Dual-Font Demo</h2>    <p style="color: #666; margin-bottom: 1rem;">
      SignWriting symbols use two overlapping fonts: SuttonSignWritingFill (white interior) and SuttonSignWritingLine (black outline)
    </p>
    <div style="display: flex; align-items: center; gap: 1.5rem;">
      <SymbolSVG symbol={demoSymbol} scale={2} margin={4} />
      <span style="font-family: SuttonSignWriting, sans-serif; font-size: 2.5rem;">{String.fromCodePoint(0xFD830)}</span>
      <span style="font-size: 1.1rem; color: #666;">(Direct font rendering for comparison)</span>
    </div>    <!-- Official SSW Library Dual-Font Test -->
    <div style="margin-top: 1rem; padding: 1rem; background: #f5f5f5; border-radius: 4px;">
      <p><strong>✅ Working Dual-Font Implementation:</strong></p>
      
      <!-- Test FSW String rendering -->
      <div style="background: white; border: 2px solid #333; padding: 15px; margin: 10px 0; text-align: center;">
        <div id="fsw-demo-output" style="min-height: 80px; display: flex; align-items: center; justify-content: center;">
          <span style="color: #666;">Click "Generate Sign" to see dual-font rendering</span>
        </div>
      </div>
      
      <div style="margin-top: 1rem;">
        <button on:click={testOfficialSSW} style="padding: 8px 16px; background: #28a745; color: white; border: none; border-radius: 4px; cursor: pointer; margin-right: 10px;">
          Generate Sign with Official SSW
        </button>
        <button on:click={testDualFontRendering} style="padding: 8px 16px; background: #007acc; color: white; border: none; border-radius: 4px; cursor: pointer;">
          Test Custom SVG Generation
        </button>
      </div>
      
      <p style="font-size: 0.9rem; color: #666; margin-top: 0.5rem;">
        The official SSW library now generates proper dual-font SVG with SuttonSignWritingLine (black outline) and SuttonSignWritingFill (white interior).
      </p>
    </div></section>
    <div class="toolbar-section">
    <Toolbar on:action={e => handleToolbarAction(e.detail)} />
  </div>
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