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
 * SignWriting Dual-Font Rendering System
 * 
 * SignWriting symbols require two overlapping fonts to render correctly:
 * 1. SuttonSignWritingFill - Provides white interior/fill
 * 2. SuttonSignWriting (Line) - Provides black outline/stroke
 * 
 * Both fonts are rendered simultaneously in the same SVG, with the fill font
 * drawn first (as a base layer) and the line font drawn on top (as an outline).
 * This creates properly formed SignWriting symbols with white interiors and
 * black outlines.
 */

/**
 * Generate SVG for a SignWriting symbol using the dual-font approach
 * This creates two overlapping text elements: one with SuttonSignWritingFill (white) 
 * and one with SuttonSignWriting (black outline), following the official implementation
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
    
    // Calculate size - use provided size or default
    const baseSize = options.size || symbol.size || 30;
    const scale = options.scale || 1;
    const finalSize = Math.round(baseSize * scale);
    
    // Create SVG with dual-font approach following official implementation
    // Structure: <g> group containing two <text> elements
    // 1. sym-fill: SuttonSignWritingFill font with ssw.pua() content
    // 2. sym-line: SuttonSignWriting font with ssw.code() content
    const fillChar = window.ssw.pua(symbolKey);
    const lineChar = window.ssw.code(symbolKey);
    
    console.log('Symbol:', symbolKey, 'Fill char:', fillChar, 'Line char:', lineChar);
    
    const svg = `<svg width="${finalSize}" height="${finalSize}" viewBox="0 0 ${finalSize} ${finalSize}" xmlns="http://www.w3.org/2000/svg">
      <defs>
        <style>
          .sym-fill {
            font-family: 'SuttonSignWritingFill';
            font-size: ${finalSize}px;
            fill: ${options.fill || symbol.nwcolor || '#ffffff'};
            text-anchor: middle;
            dominant-baseline: central;
            pointer-events: none;
          }          .sym-line {
            font-family: 'SuttonSignWritingLine';
            font-size: ${finalSize}px;
            fill: ${options.line || symbol.nbcolor || '#000000'};
            text-anchor: middle;
            dominant-baseline: central;
            pointer-events: none;
          }
        </style>
      </defs>
      <g transform="translate(${finalSize/2},${finalSize/2})">
        <text x="0" y="0" class="sym-fill">${fillChar}</text>
        <text x="0" y="0" class="sym-line">${lineChar}</text>
      </g>
    </svg>`;
    
    console.log('Generated dual-font SVG for', symbolKey);
    
    if (!svg || svg.trim() === '') {
      console.warn('Empty SVG generated for symbol:', symbolKey);
      throw new Error('Empty SVG generated');
    }
    
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
 * Generate SVG for a complete FSW (Formal SignWriting) string using dual-font approach
 * This creates a complete sign with multiple symbols positioned correctly
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
    
    // Use the official ssw.svg() to get the base structure, but we'll modify it for dual-font
    const officialSVG = window.ssw.svg(fsw);
    
    if (!officialSVG || !officialSVG.includes('<svg')) {
      throw new Error('Failed to generate SVG from FSW');
    }
    
    console.log('Original SVG:', officialSVG);
    
    // Parse the FSW to extract symbols and positions
    const symbolMatches = fsw.match(/S[0-9a-f]{5}\d+x\d+/gi);
    
    if (!symbolMatches) {
      throw new Error('No positioned symbols found in FSW');
    }
    
    // Extract viewBox and dimensions from original SVG
    const viewBoxMatch = officialSVG.match(/viewBox="([^"]+)"/);
    const widthMatch = officialSVG.match(/width="([^"]+)"/);
    const heightMatch = officialSVG.match(/height="([^"]+)"/);
    
    const viewBox = viewBoxMatch ? viewBoxMatch[1] : '0 0 100 100';
    const width = widthMatch ? widthMatch[1] : '100';
    const height = heightMatch ? heightMatch[1] : '100';
    
    // Generate dual-font version
    let dualFontSVG = `<svg width="${width}" height="${height}" viewBox="${viewBox}" xmlns="http://www.w3.org/2000/svg">`;
    
    // Add CSS for dual-font approach
    dualFontSVG += `
      <defs>
        <style>
          .sym-fill {
            font-family: 'SuttonSignWritingFill';
            fill: ${options.fill || '#ffffff'};
            text-anchor: middle;
            dominant-baseline: central;
            pointer-events: none;
          }          .sym-line {
            font-family: 'SuttonSignWritingLine';
            fill: ${options.line || '#000000'};
            text-anchor: middle;
            dominant-baseline: central;
            pointer-events: none;
          }
        </style>
      </defs>`;
    
    // Process each symbol
    symbolMatches.forEach((symbolPos) => {
      try {
        const symbolKey = symbolPos.slice(0, 6); // Extract S##### part
        const coords = symbolPos.slice(6); // Extract coordinates
        const [x, y] = coords.split('x').map(n => parseInt(n));
        
        // Get both font characters
        const fillChar = window.ssw.pua(symbolKey);
        const lineChar = window.ssw.code(symbolKey);
        
        // Get symbol size for font-size calculation
        const symbolSize = window.ssw.size(symbolKey);
        let fontSize = 30;
        if (symbolSize) {
          const [w, h] = symbolSize.split('x').map(n => parseInt(n));
          fontSize = Math.max(w, h) * (options.scale || 1);
        }
        
        // Add dual-font group for this symbol
        dualFontSVG += `
          <g transform="translate(${x},${y})">
            <text x="0" y="0" class="sym-fill" style="font-size:${fontSize}px">${fillChar}</text>
            <text x="0" y="0" class="sym-line" style="font-size:${fontSize}px">${lineChar}</text>
          </g>`;
          
      } catch (error) {
        console.warn('Failed to process symbol:', symbolPos, error);
      }
    });
    
    dualFontSVG += '</svg>';
    
    console.log('Generated dual-font FSW SVG');
    return dualFontSVG;
    
  } catch (error) {
    console.warn('Failed to generate dual-font FSW SVG:', String(error));
    
    // Fallback to official SSW SVG
    try {
      await loadSswLibrary();
      return window.ssw.svg(fsw);
    } catch (fallbackError) {
      throw new Error(`Failed to generate FSW SVG: ${String(error)}`);
    }
  }
}
