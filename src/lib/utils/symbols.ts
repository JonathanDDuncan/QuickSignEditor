/**
 * @fileoverview Pure symbol manipulation functions
 * All functions are under 20 lines and side-effect free
 */

/**
 * Creates a new symbol with unique ID
 * @param {string} key - Symbol key identifier
 * @param {number} x - X coordinate
 * @param {number} y - Y coordinate
 * @param {number} id - Unique identifier
 * @returns {Object} New symbol object
 */
export function createSymbol(key, x, y, id) {
  return {
    key: key || '',
    x: x || 0,
    y: y || 0,
    width: 30, // Default width
    height: 30, // Default height
    id: id || 0,
    selected: false,
    rotation: 0,
    fill: 1
  };
}

/**
 * Selects/deselects a symbol by ID
 * @param {Array} symbols - Array of symbols
 * @param {number} symbolId - ID of symbol to toggle
 * @param {boolean} selected - Selection state
 * @returns {Array} New array with updated selection
 */
export function selectSymbol(symbols, symbolId, selected = true) {
  if (!symbols || !Array.isArray(symbols)) return [];
  
  return symbols.map(symbol => 
    symbol.id === symbolId 
      ? { ...symbol, selected }
      : symbol
  );
}

/**
 * Deselects all symbols
 * @param {Array} symbols - Array of symbols
 * @returns {Array} New array with all symbols deselected
 */
export function deselectAllSymbols(symbols) {
  if (!symbols || !Array.isArray(symbols)) return [];
  
  return symbols.map(symbol => ({ ...symbol, selected: false }));
}

/**
 * Gets all selected symbols
 * @param {Array} symbols - Array of symbols
 * @returns {Array} Array of selected symbols
 */
export function getSelectedSymbols(symbols) {
  if (!symbols || !Array.isArray(symbols)) return [];
  
  return symbols.filter(symbol => symbol.selected);
}
