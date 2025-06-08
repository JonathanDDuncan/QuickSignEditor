// Test script for the new generateFSWSVG function
import { generateFSWSVG } from './src/lib/signWritingFont.js';

async function testFSWGeneration() {
  const fswString = "AS20310S26b02S33100M521x547S33100482x483S20310506x500S26b02503x520";
  
  try {
    console.log('Testing FSW string:', fswString);
    
    const svg = await generateFSWSVG(fswString, {
      fill: '#ffffff',
      line: '#000000'
    });
    
    console.log('Generated SVG length:', svg.length);
    console.log('Contains dual-font classes:', svg.includes('sym-fill') && svg.includes('sym-line'));
    console.log('Contains SuttonSignWritingFill:', svg.includes('SuttonSignWritingFill'));
    console.log('Contains SuttonSignWriting:', svg.includes('SuttonSignWriting'));
    
    // Save the SVG to see the result
    console.log('Generated SVG:', svg.substring(0, 500) + '...');
    
  } catch (error) {
    console.error('Error generating FSW SVG:', error);
  }
}

// Run the test
testFSWGeneration();
