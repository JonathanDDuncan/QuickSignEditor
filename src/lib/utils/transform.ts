/**
 * @fileoverview Pure transformation functions for SignWriting data
 * All functions are under 20 lines and side-effect free
 */

/**
 * Normalizes FSW coordinates to center position
 * @param {string} fsw - FSW string to normalize
 * @param {number} centerX - Center X coordinate
 * @param {number} centerY - Center Y coordinate
 * @returns {string} Normalized FSW string
 */
export function normalizeFsw(fsw, centerX = 500, centerY = 500) {
  if (!fsw) return '';
  
  // Extract coordinates and normalize them
  const coordPattern = /(\d{3})x(\d{3})/g;
  return fsw.replace(coordPattern, (match, x, y) => {
    const newX = parseInt(x) + centerX - 500;
    const newY = parseInt(y) + centerY - 500;
    return `${newX.toString().padStart(3, '0')}x${newY.toString().padStart(3, '0')}`;
  });
}

/**
 * Transforms symbol position relative to canvas
 * @param {Object} symbol - Symbol with x, y coordinates
 * @param {Object} offset - Offset {x, y} to apply
 * @returns {Object} New symbol with transformed position
 */
export function transformSymbolPosition(symbol, offset) {
  if (!symbol || !offset) return symbol;
  
  return {
    ...symbol,
    x: symbol.x + offset.x,
    y: symbol.y + offset.y
  };
}

/**
 * Transforms multiple symbols with same offset
 * @param {Array} symbols - Array of symbols to transform
 * @param {Object} offset - Offset {x, y} to apply
 * @returns {Array} Array of transformed symbols
 */
export function transformSymbols(symbols, offset) {
  if (!symbols || !Array.isArray(symbols)) return [];
  if (!offset) return symbols;
  
  return symbols.map(symbol => transformSymbolPosition(symbol, offset));
}

/**
 * Scales symbol size by factor
 * @param {Object} symbol - Symbol to scale
 * @param {number} scaleFactor - Factor to scale by (1.0 = no change)
 * @returns {Object} Scaled symbol
 */
export function scaleSymbol(symbol, scaleFactor) {
  if (!symbol || typeof scaleFactor !== 'number') return symbol;
  
  return {
    ...symbol,
    width: Math.round(symbol.width * scaleFactor),
    height: Math.round(symbol.height * scaleFactor)
  };
}
