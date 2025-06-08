// Pure function: copySymbols
// Returns a deep copy of the provided symbols array

import type { Symbol } from './types';

export function copySymbols(symbols: symbol[]): symbol[] {
  if (!Array.isArray(symbols)) return [];
  return symbols.map(s => ({ ...s }));
}

// Pure function: alignSymbols
// Aligns all symbols to the leftmost x or topmost y
export function alignSymbols(symbols: symbol[], direction: 'left' | 'top' = 'left'): symbol[] {
  if (!Array.isArray(symbols) || symbols.length === 0) return [];
  if (direction === 'left') {
    const minX = Math.min(...symbols.map(s => s.x));
    return symbols.map(s => ({ ...s, x: s.x - minX }));
  } else {
    const minY = Math.min(...symbols.map(s => s.y));
    return symbols.map(s => ({ ...s, y: s.y - minY }));
  }
}

// For Vitest compatibility, also export as default
export default { copySymbols, alignSymbols };
