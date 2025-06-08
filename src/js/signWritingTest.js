// Manual verification of dual-font SignWriting implementation
// This script can be run in the browser console to test the dual-font system

console.log('=== SignWriting Dual-Font Verification ===');

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

// Test 2: Generate and verify dual-font SVG
async function testDualFontSVG() {
  console.log('Testing dual-font SVG generation...');
  
  // Wait for generateSymbolSVG to be available
  let attempts = 0;
  while (typeof window.generateSymbolSVG === 'undefined' && attempts < 20) {
    console.log('Waiting for generateSymbolSVG... attempt', attempts + 1);
    await new Promise(resolve => setTimeout(resolve, 100));
    attempts++;
  }
  
  if (typeof window.generateSymbolSVG === 'undefined') {
    console.warn('generateSymbolSVG not available after waiting');
    return null;
  }
  
  const testSymbol = {
    key: 'S10000',
    width: 40,
    height: 40,
    size: 1
  };
  
  try {
    const svg = await window.generateSymbolSVG(testSymbol, { size: 50 });
    console.log('Generated SVG length:', svg.length);
    
    // Check for dual-font elements
    const hasFillClass = svg.includes('class="sw-fill"');
    const hasLineClass = svg.includes('class="sw-line"');
    const hasFillFont = svg.includes('SuttonSignWritingFill');
    const hasLineFont = svg.includes('SuttonSignWriting');
    
    console.log('Dual-font verification:');
    console.log('- Has fill class:', hasFillClass);
    console.log('- Has line class:', hasLineClass);
    console.log('- Has fill font:', hasFillFont);
    console.log('- Has line font:', hasLineFont);
    
    // Count Unicode characters (should be 2 for dual-font)
    const unicodeMatches = (svg.match(/󽠰/g) || []).length;
    console.log('- Unicode characters found:', unicodeMatches);
    
    return {
      hasFillClass,
      hasLineClass,
      hasFillFont,
      hasLineFont,
      unicodeCount: unicodeMatches,
      svgPreview: svg.substring(0, 200) + '...'
    };
  } catch (error) {
    console.error('SVG generation failed:', error);
    return null;
  }
}

// Test 3: Visual rendering test
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
  `;
  
  testContainer.innerHTML = `
    <h4>Dual-Font Test</h4>
    <div style="position: relative; display: inline-block; font-size: 40px;">
      <span style="font-family: SuttonSignWritingFill; color: white; position: absolute; top: 0; left: 0;">󽠰</span>
      <span style="font-family: SuttonSignWriting; color: black;">󽠰</span>
    </div>
    <p style="font-size: 12px;">Above should show a hand symbol with white fill and black outline</p>
    <button onclick="this.parentElement.remove()">Close</button>
  `;
  
  document.body.appendChild(testContainer);
  
  return testContainer;
}

// Run all tests
async function runAllTests() {
  console.log('Starting comprehensive dual-font verification...');
  
  const fontCheck = checkFontsLoaded();
  console.log('Font check results:', fontCheck);
  
  const svgCheck = await testDualFontSVG();
  console.log('SVG check results:', svgCheck);
  
  const visualTest = testVisualRendering();
  console.log('Visual test created');
  
  // Summary
  console.log('=== VERIFICATION SUMMARY ===');
  console.log('Fonts loaded:', fontCheck);
  console.log('SVG generation:', svgCheck ? 'SUCCESS' : 'FAILED');
  console.log('Visual test:', visualTest ? 'CREATED' : 'FAILED');
  
  return {
    fontCheck,
    svgCheck,
    visualTest: !!visualTest
  };
}

// Export for use
window.signWritingTest = {
  checkFonts: checkFontsLoaded,
  testSVG: testDualFontSVG,
  testVisual: testVisualRendering,
  runAll: runAllTests
};

console.log('SignWriting verification tools loaded. Run window.signWritingTest.runAll() to test everything.');
