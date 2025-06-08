/**
 * @fileoverview Tests for transformation functions
 * 100% coverage for all pure transformation functions
 */

import { describe, it, expect } from 'vitest';
import { 
  normalizeFsw,
  transformSymbolPosition,
  transformSymbols,
  scaleSymbol
} from '../lib/utils/transform.js';

describe('normalizeFsw', () => {
  it('normalizes FSW coordinates correctly', () => {
    const fsw = 'M500x500S10000490x510';
    const result = normalizeFsw(fsw, 600, 600);
    expect(result).toBe('M600x600S10000590x610');
  });

  it('handles empty FSW string', () => {
    const result = normalizeFsw('');
    expect(result).toBe('');
  });

  it('handles null FSW input', () => {
    const result = normalizeFsw(null);
    expect(result).toBe('');
  });

  it('uses default center coordinates', () => {
    const fsw = 'M500x500';
    const result = normalizeFsw(fsw);
    expect(result).toBe('M500x500');
  });
});

describe('transformSymbolPosition', () => {
  it('transforms symbol position correctly', () => {
    const symbol = { x: 100, y: 200, key: 'S10000' };
    const offset = { x: 50, y: -25 };
    const result = transformSymbolPosition(symbol, offset);
    
    expect(result.x).toBe(150);
    expect(result.y).toBe(175);
    expect(result.key).toBe('S10000');
  });

  it('handles null symbol input', () => {
    const result = transformSymbolPosition(null, { x: 10, y: 10 });
    expect(result).toBeNull();
  });

  it('handles null offset input', () => {
    const symbol = { x: 100, y: 200 };
    const result = transformSymbolPosition(symbol, null);
    expect(result).toEqual(symbol);
  });
});

describe('transformSymbols', () => {
  it('transforms multiple symbols correctly', () => {
    const symbols = [
      { x: 100, y: 200 },
      { x: 150, y: 250 }
    ];
    const offset = { x: 10, y: -10 };
    const result = transformSymbols(symbols, offset);
    
    expect(result).toHaveLength(2);
    expect(result[0]).toEqual({ x: 110, y: 190 });
    expect(result[1]).toEqual({ x: 160, y: 240 });
  });

  it('handles null symbols input', () => {
    const result = transformSymbols(null, { x: 10, y: 10 });
    expect(result).toEqual([]);
  });

  it('handles non-array symbols input', () => {
    const result = transformSymbols('not-array', { x: 10, y: 10 });
    expect(result).toEqual([]);
  });

  it('handles null offset input', () => {
    const symbols = [{ x: 100, y: 200 }];
    const result = transformSymbols(symbols, null);
    expect(result).toEqual(symbols);
  });
});

describe('scaleSymbol', () => {
  it('scales symbol correctly', () => {
    const symbol = { width: 100, height: 50, key: 'S10000' };
    const result = scaleSymbol(symbol, 1.5);
    
    expect(result.width).toBe(150);
    expect(result.height).toBe(75);
    expect(result.key).toBe('S10000');
  });

  it('rounds scaled dimensions', () => {
    const symbol = { width: 100, height: 100 };
    const result = scaleSymbol(symbol, 1.33);
    
    expect(result.width).toBe(133);
    expect(result.height).toBe(133);
  });

  it('handles null symbol input', () => {
    const result = scaleSymbol(null, 1.5);
    expect(result).toBeNull();
  });

  it('handles non-numeric scale factor', () => {
    const symbol = { width: 100, height: 50 };
    const result = scaleSymbol(symbol, 'not-number');
    expect(result).toEqual(symbol);
  });
});
