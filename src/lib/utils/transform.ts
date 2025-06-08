import type { Symbol } from './types';

// Pure function: transformSymbols
// Example: center all symbols on a 500x500 canvas
// Accepts array of symbols with x/y, returns new array with transformed positions

export function transformSymbols(symbols: symbol[], options?: { center?: boolean }): symbol[] {
  if (!Array.isArray(symbols)) return [];
  if (!options?.center) return symbols;
  // Find bounding box
  const minX = Math.min(...symbols.map(s => s.x));
  const minY = Math.min(...symbols.map(s => s.y));
  const maxX = Math.max(...symbols.map(s => s.x));
  const maxY = Math.max(...symbols.map(s => s.y));
  const offsetX = 250 - (minX + (maxX - minX) / 2);
  const offsetY = 250 - (minY + (maxY - minY) / 2);
  return symbols.map(s => ({ ...s, x: s.x + offsetX, y: s.y + offsetY }));
}

// For Vitest compatibility, also export as default
export default transformSymbols;
