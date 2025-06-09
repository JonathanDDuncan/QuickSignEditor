// SignWriting font utilities using the existing SuttonSignWriting.js library
// Note: The ssw object will be available globally after loading SuttonSignWriting.js

// Declare the global ssw object from SuttonSignWriting.js
declare global {
  interface Window {
    ssw: {
      svg: (text: string, options?: any) => string;
      size: (text: string) => string;
      key: (text: string, style?: boolean) => string;
      fsw: (text: string, style?: boolean) => string;
      colorize: (key: string) => string;
      code: (text: string, hexval?: boolean) => string;
      pua: (text: string, hexval?: boolean) => string;
    };
  }
}

export interface SignWritingSymbol {
  key: string;
  unicodepua?: string;
  width?: number;
  height?: number;
  size?: number;
  nwcolor?: string; // fill color
  nbcolor?: string; // line color
}

/**
 * Check if SuttonSignWriting.js library is loaded
 */
function isSswLoaded(): boolean {
  return typeof window !== 'undefined' && 
         typeof window.ssw !== 'undefined' && 
         typeof window.ssw.svg === 'function';
}

/**
 * Load the SuttonSignWriting.js library dynamically if not already loaded
 */
async function loadSswLibrary(): Promise<void> {
  if (isSswLoaded()) {
    return Promise.resolve();
  }

  // Wait for library to be available (it should load from index.html)
  return new Promise((resolve, reject) => {
    const maxAttempts = 50; // 5 seconds max
    let attempts = 0;
    
    const checkLibrary = () => {
      attempts++;
      console.log(`Checking for SSW library... attempt ${attempts}`);
      
      if (isSswLoaded()) {
        console.log('SSW library found!');
        resolve();
      } else if (attempts >= maxAttempts) {
        console.error('SSW library not found after max attempts');
        reject(new Error('SuttonSignWriting library not available'));
      } else {
        setTimeout(checkLibrary, 100);
      }
    };
    
    checkLibrary();
  });
}

/**
 * SignWriting Symbol Rendering System
 * 
 * This module provides functions to render SignWriting symbols using the
 * official SuttonSignWriting.js library. The ssw.svg() function generates
 * properly formatted SVG elements that display SignWriting symbols correctly.
 */

/**
 * Generate SVG for a SignWriting symbol using the simple ssw.svg() approach
 * This uses the official SSW library function that works reliably
 */
export async function generateSymbolSVG(symbol: SignWritingSymbol, options: {
  scale?: number;
  size?: number;
  line?: string;
  fill?: string;
} = {}): Promise<string> {
  try {
    // Ensure the library is loaded
    await loadSswLibrary();
    
    const symbolKey = symbol.key || '';
    
    if (!symbolKey) {
      throw new Error('No symbol key provided');
    }
    
    // Use the official ssw.svg() function directly - this works reliably
    const svg = window.ssw.svg(symbolKey);
    
    if (!svg || !svg.includes('<svg')) {
      throw new Error('Failed to generate SVG from symbol key');
    }
    
    console.log('Generated SVG for symbol:', symbolKey, '- Length:', svg.length);
    
    return svg;
    
  } catch (error) {
    console.warn('Failed to generate SVG for symbol:', symbol.key, String(error));
    const size = options.size || symbol.size || 40;
    return `<svg width="${size}" height="${size}" viewBox="0 0 ${size} ${size}" xmlns="http://www.w3.org/2000/svg">
      <rect width="100%" height="100%" fill="none" stroke="#ccc" stroke-dasharray="2,2"/>
      <text x="50%" y="50%" text-anchor="middle" font-size="${size * 0.3}" fill="#666">?</text>
    </svg>`;
  }
}

/**
 * Generate PNG data URL for a SignWriting symbol
 * Note: This converts SVG to PNG using canvas
 */
export async function generateSymbolPNG(symbol: SignWritingSymbol, options: {
  scale?: number;
  size?: number;
  line?: string;
  fill?: string;
} = {}): Promise<string> {
  try {
    // For now, we'll convert SVG to canvas and then to PNG
    const svg = await generateSymbolSVG(symbol, options);
    
    // Create a canvas and draw the SVG
    const canvas = document.createElement('canvas');
    const ctx = canvas.getContext('2d');
    const size = options.size || symbol.size || 40;
    
    canvas.width = size;
    canvas.height = size;
    
    // Create an image from the SVG
    const img = new Image();
    const svgBlob = new Blob([svg], { type: 'image/svg+xml' });
    const url = URL.createObjectURL(svgBlob);
    
    return new Promise((resolve, reject) => {
      img.onload = () => {
        if (ctx) {
          ctx.drawImage(img, 0, 0, size, size);
          const pngDataUrl = canvas.toDataURL('image/png');
          URL.revokeObjectURL(url);
          resolve(pngDataUrl);
        } else {
          reject(new Error('Canvas context not available'));
        }
      };
      img.onerror = () => {
        URL.revokeObjectURL(url);
        reject(new Error('Failed to load SVG image'));
      };
      img.src = url;
    });
  } catch (error) {
    console.warn('Failed to generate PNG for symbol:', symbol.key, String(error));
    return 'data:image/png;base64,';
  }
}

/**
 * Load SignWriting fonts using the SuttonSignWriting.css
 */
export async function loadSignWritingFonts(): Promise<void> {
  // Load the library first
  await loadSswLibrary();
  
  // The fonts should be loaded via the CSS file that's already included
  // Check if fonts are available by testing if we can get symbol sizes
  return new Promise((resolve, reject) => {
    const checkFonts = () => {
      try {
        // Test with a known symbol
        const testSymbol = 'S10000';
        const size = window.ssw.size(testSymbol);
        if (size && size !== '') {
          resolve();
        } else {
          // Retry after a short delay
          setTimeout(checkFonts, 100);
        }
      } catch (error) {
        // Retry after a short delay
        setTimeout(checkFonts, 100);
      }
    };
    
    // Start checking after a brief delay to allow fonts to load
    setTimeout(checkFonts, 100);
    
    // Timeout after 5 seconds
    setTimeout(() => {
      reject(new Error('SignWriting fonts failed to load within 5 seconds'));
    }, 5000);
  });
}

/**
 * Generate SVG for a complete FSW (Formal SignWriting) string
 * Uses the official ssw.svg() function which works correctly with the SSW library
 */
export async function generateFSWSVG(fsw: string, options: {
  scale?: number;
  size?: number;
  line?: string;
  fill?: string;
} = {}): Promise<string> {
  try {
    // Ensure the library is loaded
    await loadSswLibrary();
    
    if (!fsw || typeof fsw !== 'string') {
      throw new Error('No FSW string provided');
    }
    
    // Use the official ssw.svg() function directly - this works in all the test files
    const svg = window.ssw.svg(fsw);
    
    if (!svg || !svg.includes('<svg')) {
      throw new Error('Failed to generate SVG from FSW');
    }
    
    console.log('Generated SVG for FSW:', fsw, '- Length:', svg.length);
    
    return svg;
    
  } catch (error) {
    console.warn('Failed to generate FSW SVG:', String(error));
    throw new Error(`Failed to generate FSW SVG: ${String(error)}`);
  }
}

/**
 * Synchronous version of generateFSWSVG for cases where the SSW library is already loaded
 * This is more efficient for components that render many symbols
 */
export function generateFSWSVGSync(fsw: string, options: {
  scale?: number;
  size?: number;
  line?: string;
  fill?: string;
} = {}): string {
  try {
    console.log('generateFSWSVGSync called with FSW:', fsw, 'options:', options);
    
    if (!isSswLoaded()) {
      console.error('SSW library not loaded - window.ssw is:', typeof window !== 'undefined' ? typeof window.ssw : 'window undefined');
      throw new Error('SSW library not loaded - use async version');
    }
    
    if (!fsw || typeof fsw !== 'string') {
      console.error('No FSW string provided:', fsw);
      throw new Error('No FSW string provided');
    }
    
    // Use the official ssw.svg() function directly - this works in all the test files
    console.log('Calling window.ssw.svg with FSW:', fsw);
    console.log('window.ssw.svg function available:', typeof window.ssw.svg === 'function');
    
    const svg = window.ssw.svg(fsw);
    
    console.log('window.ssw.svg returned:', typeof svg, 'length:', svg?.length);
    
    if (!svg || !svg.includes('<svg')) {
      console.error('Failed to generate SVG from FSW:', fsw, 'result:', svg);
      throw new Error('Failed to generate SVG from FSW');
    }
    
    console.log('Successfully generated SVG for FSW:', fsw, '- SVG length:', svg.length);
    console.log('SVG preview (first 200 chars):', svg.substring(0, 200));
    
    return svg;
    
  } catch (error) {
    console.warn('Failed to generate FSW SVG (sync):', String(error));
    console.log('Returning placeholder SVG');
    // Return a placeholder SVG instead of throwing
    return `<svg width="40" height="40" viewBox="0 0 40 40" xmlns="http://www.w3.org/2000/svg">
      <rect width="100%" height="100%" fill="none" stroke="#ccc" stroke-dasharray="2,2"/>
      <text x="20" y="24" text-anchor="middle" font-size="12" fill="#666">?</text>
    </svg>`;
  }
}
