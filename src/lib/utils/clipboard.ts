/**
 * @fileoverview Pure clipboard operations for symbols
 * All functions are under 20 lines and side-effect free
 */

/**
 * Copies selected symbols to clipboard data
 * @param {Array} symbols - Array of symbols
 * @returns {Array} Array of selected symbols for clipboard
 */
export function copySelectedSymbols(symbols) {
  if (!symbols || !Array.isArray(symbols)) return [];
  
  return symbols
    .filter(symbol => symbol.selected)
    .map(symbol => ({
      ...symbol,
      selected: false // Deselect when copying
    }));
}

/**
 * Prepares symbols for pasting at position
 * @param {Array} clipboardSymbols - Symbols from clipboard
 * @param {Object} position - Target position {x, y}
 * @param {number} nextId - Next available ID
 * @returns {Array} Array of symbols ready for pasting
 */
export function preparePasteSymbols(clipboardSymbols, position, nextId) {
  if (!clipboardSymbols || !Array.isArray(clipboardSymbols)) return [];
  if (!position) position = { x: 0, y: 0 };
  
  // Calculate offset from first symbol
  const firstSymbol = clipboardSymbols[0];
  if (!firstSymbol) return [];
  
  const offsetX = position.x - firstSymbol.x;
  const offsetY = position.y - firstSymbol.y;
  
  return clipboardSymbols.map((symbol, index) => ({
    ...symbol,
    x: symbol.x + offsetX,
    y: symbol.y + offsetY,
    id: nextId + index,
    selected: true
  }));
}

/**
 * Aligns symbols horizontally or vertically
 * @param {Array} symbols - Array of symbols to align
 * @param {'horizontal' | 'vertical' | 'center'} alignment - Alignment type
 * @returns {Array} Array of aligned symbols
 */
export function alignSymbols(symbols, alignment) {
  if (!symbols || !Array.isArray(symbols)) return [];
  
  const selectedSymbols = symbols.filter(s => s.selected);
  if (selectedSymbols.length < 2) return symbols;
  
  const bounds = selectedSymbols.reduce((acc, s) => ({
    minX: Math.min(acc.minX, s.x),
    maxX: Math.max(acc.maxX, s.x + s.width),
    minY: Math.min(acc.minY, s.y),
    maxY: Math.max(acc.maxY, s.y + s.height)
  }), { minX: Infinity, maxX: -Infinity, minY: Infinity, maxY: -Infinity });
  
  const centerX = bounds.minX + (bounds.maxX - bounds.minX) / 2;
  const centerY = bounds.minY + (bounds.maxY - bounds.minY) / 2;
  
  return symbols.map(symbol => {
    if (!symbol.selected) return symbol;
    
    switch (alignment) {
      case 'horizontal':
        return { ...symbol, y: centerY - symbol.height / 2 };
      case 'vertical':
        return { ...symbol, x: centerX - symbol.width / 2 };
      case 'center':
        return { 
          ...symbol, 
          x: centerX - symbol.width / 2,
          y: centerY - symbol.height / 2
        };
      default:
        return symbol;
    }
  });
}
