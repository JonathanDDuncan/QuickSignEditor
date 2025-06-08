import { describe, it, expect } from 'vitest';
import { copySymbols, alignSymbols } from './clipboard';
import type { Symbol } from './types';

describe('copySymbols', () => {
  it('returns a deep copy of the symbols array', () => {
    const symbols: Symbol[] = [
      { id: 'a', x: 1, y: 2 },
      { id: 'b', x: 3, y: 4 }
    ];
    const result = copySymbols(symbols);
    expect(result).toEqual(symbols);
    expect(result).not.toBe(symbols);
    expect(result[0]).not.toBe(symbols[0]);
  });

  it('returns empty array for invalid input', () => {
    expect(copySymbols(null as unknown as Symbol[])).toEqual([]);
    expect(copySymbols(undefined as unknown as Symbol[])).toEqual([]);
  });
});

describe('alignSymbols', () => {
  const symbols: Symbol[] = [
    { id: 'a', x: 10, y: 20 },
    { id: 'b', x: 30, y: 40 }
  ];

  it('aligns symbols to the left', () => {
    const result = alignSymbols(symbols, 'left');
    expect(result[0].x).toBe(0);
    expect(result[1].x).toBe(20);
  });

  it('aligns symbols to the top', () => {
    const result = alignSymbols(symbols, 'top');
    expect(result[0].y).toBe(0);
    expect(result[1].y).toBe(20);
  });

  it('returns empty array for invalid input', () => {
    expect(alignSymbols(null as unknown as Symbol[])).toEqual([]);
    expect(alignSymbols(undefined as unknown as Symbol[])).toEqual([]);
    expect(alignSymbols([], 'left')).toEqual([]);
  });

  // Add a dummy test to ensure Vitest recognizes the suite
  it('dummy test', () => {
    expect(true).toBe(true);
  });
});
