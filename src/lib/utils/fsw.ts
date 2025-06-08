/**
 * @fileoverview Pure FSW parsing and manipulation functions
 * All functions are under 20 lines and side-effect free
 */

/**
 * Extracts symbol keys from FSW string
 * @param {string} fsw - FSW format string
 * @returns {Array} Array of symbol key strings
 */
export function extractSymbolKeys(fsw: string): string[] {
  if (!fsw || typeof fsw !== 'string') return [];
  
  // Match symbol pattern: S followed by hex digits
  const symbolPattern = /S[123][0-9a-f]{2}[0-5][0-9a-f]/g;
  return fsw.match(symbolPattern) || [];
}

/**
 * Extracts coordinates from FSW string
 * @param {string} fsw - FSW format string
 * @returns {Array} Array of coordinate objects {x, y}
 */
export function extractCoordinates(fsw: string): {x: number, y: number}[] {
  if (!fsw || typeof fsw !== 'string') return [];
  
  const coordPattern = /(\d{3})x(\d{3})/g;
  const matches = [];
  let match;
  
  while ((match = coordPattern.exec(fsw)) !== null) {
    matches.push({
      x: parseInt(match[1]),
      y: parseInt(match[2])
    });
  }
  
  return matches;
}

/**
 * Parses FSW into structured sign data
 * @param {string} fsw - FSW format string
 * @returns {Object} Parsed sign object
 */
export function parseFsw(fsw: string): { symbols: string[], coordinates: {x: number, y: number}[], raw: string } {
  if (!fsw) return { symbols: [], coordinates: [] } as any;
  
  const symbols = extractSymbolKeys(fsw);
  const coordinates = extractCoordinates(fsw);
  
  return {
    symbols,
    coordinates,
    raw: fsw
  };
}

/**
 * Builds FSW string from symbol data
 * @param {Array} symbols - Array of symbol objects
 * @returns {string} FSW format string
 */
export function buildFsw(symbols: any[]): string {
  if (!symbols || !Array.isArray(symbols)) return '';
  
  const parts = symbols.map(symbol => {
    const x = symbol.x.toString().padStart(3, '0');
    const y = symbol.y.toString().padStart(3, '0');
    return `${symbol.key}${x}x${y}`;
  });
  
  return parts.join('');
}

// Aliases for test and code compatibility
export const parseFSW = parseFsw;
export const buildFSW = buildFsw;
export const formatFSW = buildFsw; // formatFSW is an alias for buildFsw
export const generateFSW = buildFsw; // generateFSW is an alias for buildFsw
