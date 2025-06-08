import { describe, it, expect } from 'vitest';
import { transformSymbols, Symbol } from './transform';

describe('transformSymbols', () => {
  const symbols: symbol[] = [
    { id: 'a', x: 10, y: 10 },
    { id: 'b', x: 110, y: 110 },
  ];

  it('returns original array if no options', () => {
    const result = transformSymbols(symbols);
    expect(result).toEqual(symbols);
  });

  it('centers symbols on 500x500 canvas', () => {
    const result = transformSymbols(symbols, { center: true });
    // The center of bounding box is (60, 60), so offset is (190, 190)
    expect(result[0].x).toBeCloseTo(200);
    expect(result[0].y).toBeCloseTo(200);
    expect(result[1].x).toBeCloseTo(300);
    expect(result[1].y).toBeCloseTo(300);
  });

  it('returns empty array for invalid input', () => {
    expect(transformSymbols(null as unknown as import('./types').Symbol[])).toEqual([]);
    expect(transformSymbols(undefined as unknown as import('./types').Symbol[])).toEqual([]);
  });

  // Add a dummy test to ensure Vitest recognizes the suite
  it('dummy test', () => {
    expect(true).toBe(true);
  });
});
