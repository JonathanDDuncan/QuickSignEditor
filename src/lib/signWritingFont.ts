// SignWriting font utilities using the SuttonSignWriting.ts library
import ssw from './utils/SuttonSignWriting';
import type { SignWritingSymbol, SignWritingSign, SignWritingOptions } from '../types/signwriting';

// Re-export types for convenience
export type { SignWritingSymbol, SignWritingSign, SignWritingOptions } from '../types/signwriting';

export interface SignWritingSymbolOptions {
  width?: number;
  height?: number;
  size?: number;
  nwcolor?: string; // fill color
  nbcolor?: string; // line color
}

/**
 * SignWriting Symbol Rendering System
 * 
 * This module provides functions to render SignWriting symbols using the
 * official SuttonSignWriting.ts library. The ssw.svg() function generates
 * properly formatted SVG elements that display SignWriting symbols correctly.
 */

/**
 * Generate SVG for a SignWriting symbol using the ssw.svg() approach
 */
export function generateSymbolSVG(symbol: SignWritingSymbol, options: {
  scale?: number;
  size?: number;
  line?: string;
  fill?: string;
} = {}): string {
  try {
    const symbolKey = symbol.key || '';
    
    if (!symbolKey) {
      throw new Error('No symbol key provided');
    }
      // Use the official ssw.svg() function directly with options
    const svg = ssw.svg(symbolKey, options);
    
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
      <text x="${size/2}" y="${size/2 + 4}" text-anchor="middle" font-size="12" fill="#666">?</text>
    </svg>`;
  }
}

/**
 * Calculate the size of a SignWriting symbol
 */
export function calculateSymbolSize(symbolKey: string): { width: number; height: number } {
  try {
    if (!symbolKey) {
      return { width: 40, height: 40 };
    }
    
    // Use ssw.size() if available
    if (typeof ssw.size === 'function') {
      const size = ssw.size(symbolKey);
      console.log('Symbol size for', symbolKey, ':', size);
      return size || { width: 40, height: 40 };
    }
    
    return { width: 40, height: 40 };
  } catch (error) {
    console.warn('Failed to calculate symbol size:', symbolKey, String(error));
    return { width: 40, height: 40 };
  }
}

/**
 * Generate SVG for a FSW (Formal SignWriting) string
 */
export function generateFSWSVG(fsw: string, options: {
  scale?: number;
  size?: number;
  line?: string;
  fill?: string;
} = {}): string {
  try {
    if (!fsw || typeof fsw !== 'string') {
      throw new Error('No FSW string provided');
    }
      // Use the official ssw.svg() function directly with options
    const svg = ssw.svg(fsw, options);
    
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
 * Synchronous version of generateFSWSVG 
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
    
    if (!fsw || typeof fsw !== 'string') {
      console.error('No FSW string provided:', fsw);
      throw new Error('No FSW string provided');
    }
      // Use the official ssw.svg() function directly with options
    console.log('Calling ssw.svg with FSW:', fsw);
    console.log('ssw.svg function available:', typeof ssw.svg === 'function');
    
    const svg = ssw.svg(fsw, options);
    
    console.log('ssw.svg returned:', typeof svg, 'length:', svg?.length);
    
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

/**
 * Generate SVG for a sign (collection of symbols with positions)
 */
export function generateSignSVG(sign: SignWritingSign, options: SignWritingOptions = {}): string {
  try {
    if (!sign || !sign.symbols || sign.symbols.length === 0) {
      throw new Error('No sign data provided');
    }
    
    // Convert sign to FSW format if needed
    let fsw = '';
    if (typeof sign === 'string') {
      fsw = sign;
    } else {
      // Build FSW string from symbols      // Generate FSW string from symbols (simplified version)
      fsw = sign.symbols.map(sym => sym.key).join('');
    }
    
    return generateFSWSVG(fsw, options);
    
  } catch (error) {
    console.warn('Failed to generate sign SVG:', String(error));
    return `<svg width="100" height="100" viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg">
      <rect width="100%" height="100%" fill="none" stroke="#ccc" stroke-dasharray="2,2"/>
      <text x="50" y="54" text-anchor="middle" font-size="12" fill="#666">Sign Error</text>
    </svg>`;
  }
}

/**
 * Generate SVG with styling options
 */
export function generateStyledSVG(input: string, style: string = ''): string {
  try {
    if (!input) {
      throw new Error('No input provided');
    }
    
    // Apply style if provided
    const styleOptions = parseStyleString(style);
    const svg = ssw.svg(input, styleOptions);
    
    if (!svg || !svg.includes('<svg')) {
      throw new Error('Failed to generate styled SVG');
    }
    
    return svg;
    
  } catch (error) {
    console.warn('Failed to generate styled SVG:', String(error));
    return generateFSWSVG(input);
  }
}

/**
 * Parse style string into options object
 */
function parseStyleString(style: string): any {
  const options: any = {};
  
  if (!style) return options;
  
  // Parse common style options
  if (style.includes('fill:')) {
    const fillMatch = style.match(/fill:\s*([^;]+)/);
    if (fillMatch) options.fill = fillMatch[1].trim();
  }
  
  if (style.includes('stroke:')) {
    const strokeMatch = style.match(/stroke:\s*([^;]+)/);
    if (strokeMatch) options.line = strokeMatch[1].trim();
  }
  
  return options;
}

/**
 * Get symbol information
 */
export function getSymbolInfo(symbolKey: string): any {
  try {
    if (!symbolKey) return null;
    
    const info: any = { key: symbolKey };
    
    // Get size if available
    if (typeof ssw.size === 'function') {
      info.size = ssw.size(symbolKey);
    }
      // Get symbol information using available functions
    if (typeof ssw.symbol === 'function') {
      const symbol = ssw.symbol(symbolKey, false);
      if (symbol) info.symbol = symbol;
    }
    
    return info;
    
  } catch (error) {
    console.warn('Failed to get symbol info:', symbolKey, String(error));
    return { key: symbolKey, error: String(error) };
  }
}

/**
 * Utility function to check if a string is a valid FSW
 */
export function isValidFSW(input: string): boolean {
  try {
    if (!input || typeof input !== 'string') return false;
    
    // Use ssw.re.fsw.sign regex if available
    if (ssw.re && ssw.re.fsw && ssw.re.fsw.sign) {
      const regex = new RegExp(ssw.re.fsw.sign);
      return regex.test(input);
    }
    
    // Basic FSW pattern check
    return /^[AM]?S[0-9a-fA-F]+/.test(input);
    
  } catch (error) {
    console.warn('Failed to validate FSW:', input, String(error));
    return false;
  }
}

/**
 * Utility function to check if a string is a valid symbol key
 */
export function isValidSymbolKey(input: string): boolean {
  try {
    if (!input || typeof input !== 'string') return false;
    
    // Use ssw.re.fsw.symbol regex if available
    if (ssw.re && ssw.re.fsw && ssw.re.fsw.symbol) {
      const regex = new RegExp(ssw.re.fsw.symbol);
      return regex.test(input);
    }
    
    // Basic symbol key pattern check
    return /^S[0-9a-fA-F]{5}$/.test(input);
    
  } catch (error) {
    console.warn('Failed to validate symbol key:', input, String(error));
    return false;
  }
}
