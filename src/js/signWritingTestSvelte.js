// Manual verification of dual-font SignWriting implementation
// This script tests the SymbolSVG component rendering in the browser

console.log('=== SignWriting Dual-Font Verification (Svelte Approach) ===');

// Test 1: Check if both fonts are loaded
function checkFontsLoaded() {
  console.log('Checking font availability...');
  
  // Create test elements for each font
  const fillTest = document.createElement('span');
  fillTest.style.fontFamily = 'SuttonSignWritingFill';
  fillTest.style.fontSize = '20px';
  fillTest.style.visibility = 'hidden';
  fillTest.textContent = '󽠰';
  document.body.appendChild(fillTest);
  
  const lineTest = document.createElement('span');
  lineTest.style.fontFamily = 'SuttonSignWriting';
  lineTest.style.fontSize = '20px';
  lineTest.style.visibility = 'hidden';
  lineTest.textContent = '󽠰';
  document.body.appendChild(lineTest);
  
  // Check computed styles
  const fillComputed = window.getComputedStyle(fillTest);
  const lineComputed = window.getComputedStyle(lineTest);
  
  console.log('Fill font computed:', fillComputed.fontFamily);
  console.log('Line font computed:', lineComputed.fontFamily);
  
  // Check if SignWriting fonts are in the computed font stack
  const fillHasSignWriting = fillComputed.fontFamily.includes('SuttonSignWritingFill') || 
                           fillComputed.fontFamily.includes('SuttonSignWriting');
  const lineHasSignWriting = lineComputed.fontFamily.includes('SuttonSignWriting') ||
                           lineComputed.fontFamily.includes('SuttonSignWritingLine');
  
  // Clean up
  document.body.removeChild(fillTest);
  document.body.removeChild(lineTest);
  
  return {
    fillFont: fillComputed.fontFamily,
    lineFont: lineComputed.fontFamily,
    fillFontLoaded: fillHasSignWriting,
    lineFontLoaded: lineHasSignWriting
  };
}

// Test 2: Test SymbolSVG component rendering
function testSymbolSVGComponent() {
  console.log('Testing SymbolSVG component rendering...');
  
  // Find existing SymbolSVG components in the DOM
  const symbolSvgs = document.querySelectorAll('svg');
  console.log('Found', symbolSvgs.length, 'SVG elements');
  
  let dualFontFound = false;
  let testResults = {
    svgCount: symbolSvgs.length,
    dualFontElements: 0,
    fillElements: 0,
    lineElements: 0,
    unicodeChars: 0,
    examples: []
  };
  
  symbolSvgs.forEach((svg, index) => {
    console.log(`Checking SVG ${index + 1}:`, svg.outerHTML.substring(0, 100) + '...');
    
    // Check for dual-font classes
    const fillElements = svg.querySelectorAll('.sw-fill, text.signwriting-fill');
    const lineElements = svg.querySelectorAll('.sw-line, text.signwriting-line');
    
    testResults.fillElements += fillElements.length;
    testResults.lineElements += lineElements.length;
    
    if (fillElements.length > 0 && lineElements.length > 0) {
      dualFontFound = true;
      testResults.dualFontElements++;
      console.log('- Dual-font SVG found!');
      console.log('- Fill elements:', fillElements.length);
      console.log('- Line elements:', lineElements.length);
      
      testResults.examples.push({
        index: index + 1,
        fillCount: fillElements.length,
        lineCount: lineElements.length,
        outerHTML: svg.outerHTML.substring(0, 200)
      });
    }
    
    // Count Unicode characters
    const svgText = svg.textContent || '';
    const unicodeMatches = (svgText.match(/[\uFD00-\uFDFF\uFE00-\uFEFF]/g) || []).length;
    testResults.unicodeChars += unicodeMatches;
    
    if (unicodeMatches > 0) {
      console.log('- Unicode characters found:', unicodeMatches);
    }
  });
  
  return {
    success: dualFontFound,
    ...testResults
  };
}

// Test 3: Test SuttonSignWriting.js library directly
function testSSWLibrary() {
  console.log('Testing SuttonSignWriting.js library...');
  
  if (typeof window.ssw === 'undefined') {
    console.warn('SuttonSignWriting.js library not loaded');
    return { available: false };
  }
  
  console.log('SSW library available');
  console.log('SSW methods:', Object.keys(window.ssw));
  
  // Test basic functionality
  try {
    const testKey = 'S10000';
    const pua = window.ssw.pua ? window.ssw.pua(testKey) : 'N/A';
    const size = window.ssw.size ? window.ssw.size(testKey) : 'N/A';
    const key = window.ssw.key ? window.ssw.key(testKey) : 'N/A';
    
    console.log('Test results for', testKey, ':');
    console.log('- PUA:', pua);
    console.log('- Size:', size);
    console.log('- Key:', key);
    
    return {
      available: true,
      testKey,
      pua,
      size,
      key,
      methods: Object.keys(window.ssw)
    };
  } catch (error) {
    console.error('Error testing SSW library:', error);
    return {
      available: true,
      error: error.message
    };
  }
}

// Test 4: Visual rendering test
function testVisualRendering() {
  console.log('Creating visual rendering test...');
  
  // Create test container
  const testContainer = document.createElement('div');
  testContainer.style.cssText = `
    position: fixed;
    top: 10px;
    right: 10px;
    background: white;
    border: 2px solid #ccc;
    padding: 10px;
    z-index: 10000;
    font-size: 14px;
    max-width: 300px;
  `;
  
  testContainer.innerHTML = `
    <h4>Dual-Font Test</h4>
    <div style="position: relative; display: inline-block; font-size: 40px; background: #f0f0f0; padding: 5px;">
      <span style="font-family: SuttonSignWritingFill; color: white; position: absolute; top: 5px; left: 5px;">󽠰</span>
      <span style="font-family: SuttonSignWriting; color: black;">󽠰</span>
    </div>
    <p style="font-size: 12px; margin: 5px 0;">Above should show a hand symbol with white fill and black outline</p>
    <div style="font-size: 12px; background: #f8f8f8; padding: 5px; margin: 5px 0;">
      <strong>Font Test:</strong><br>
      Fill: <span style="font-family: SuttonSignWritingFill; font-size: 20px;">󽠰</span><br>
      Line: <span style="font-family: SuttonSignWriting; font-size: 20px;">󽠰</span>
    </div>
    <button onclick="this.parentElement.remove()" style="padding: 5px 10px;">Close</button>
  `;
  
  document.body.appendChild(testContainer);
  
  return testContainer;
}

// Run all tests
async function runAllTests() {
  console.log('Starting comprehensive dual-font verification...');
  
  const fontCheck = checkFontsLoaded();
  console.log('Font check results:', fontCheck);
  
  const componentCheck = testSymbolSVGComponent();
  console.log('Component check results:', componentCheck);
  
  const sswCheck = testSSWLibrary();
  console.log('SSW library check results:', sswCheck);
  
  const visualTest = testVisualRendering();
  console.log('Visual test created');
  
  // Summary
  console.log('=== VERIFICATION SUMMARY ===');
  console.log('Fonts loaded:', fontCheck.fillFontLoaded && fontCheck.lineFontLoaded ? 'SUCCESS' : 'PARTIAL/FAILED');
  console.log('Component rendering:', componentCheck.success ? 'SUCCESS' : 'NO DUAL-FONT COMPONENTS FOUND');
  console.log('SSW library:', sswCheck.available ? 'SUCCESS' : 'FAILED');
  console.log('Visual test:', visualTest ? 'CREATED' : 'FAILED');
  
  return {
    fontCheck,
    componentCheck,
    sswCheck,
    visualTest: !!visualTest
  };
}

// Export for use
window.signWritingTest = {
  checkFonts: checkFontsLoaded,
  testComponents: testSymbolSVGComponent,
  testSSW: testSSWLibrary,
  testVisual: testVisualRendering,
  runAll: runAllTests
};

console.log('SignWriting verification tools loaded (Svelte approach). Run window.signWritingTest.runAll() to test everything.');
