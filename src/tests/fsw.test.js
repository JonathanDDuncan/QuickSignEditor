/**
 * @fileoverview Tests for FSW parsing functions
 * 100% coverage for all pure FSW functions
 */

import { describe, it, expect } from 'vitest';
import { 
  extractSymbolKeys,
  extractCoordinates,
  parseFsw,
  buildFsw
} from '../lib/utils/fsw.js';

describe('extractSymbolKeys', () => {
  it('extracts symbol keys from FSW string', () => {
    const fsw = 'M500x500S10000490x510S20500510x490';
    const result = extractSymbolKeys(fsw);
    expect(result).toEqual(['S10000', 'S20500']);
  });

  it('handles FSW with no symbols', () => {
    const fsw = 'M500x500';
    const result = extractSymbolKeys(fsw);
    expect(result).toEqual([]);
  });

  it('handles null input', () => {
    const result = extractSymbolKeys(null);
    expect(result).toEqual([]);
  });

  it('handles non-string input', () => {
    const result = extractSymbolKeys(123);
    expect(result).toEqual([]);
  });
});

describe('extractCoordinates', () => {
  it('extracts coordinates from FSW string', () => {
    const fsw = 'M500x500S10000490x510S20500510x490';
    const result = extractCoordinates(fsw);
    expect(result).toEqual([
      { x: 500, y: 500 },
      { x: 490, y: 510 },
      { x: 510, y: 490 }
    ]);
  });

  it('handles FSW with no coordinates', () => {
    const fsw = 'S10000S20500';
    const result = extractCoordinates(fsw);
    expect(result).toEqual([]);
  });

  it('handles null input', () => {
    const result = extractCoordinates(null);
    expect(result).toEqual([]);
  });

  it('handles non-string input', () => {
    const result = extractCoordinates(123);
    expect(result).toEqual([]);
  });
});

describe('parseFsw', () => {
  it('parses complete FSW string', () => {
    const fsw = 'M500x500S10000490x510';
    const result = parseFsw(fsw);
    
    expect(result.symbols).toEqual(['S10000']);
    expect(result.coordinates).toEqual([
      { x: 500, y: 500 },
      { x: 490, y: 510 }
    ]);
    expect(result.raw).toBe(fsw);
  });

  it('handles empty FSW string', () => {
    const result = parseFsw('');
    expect(result).toEqual({ symbols: [], coordinates: [] });
  });

  it('handles null input', () => {
    const result = parseFsw(null);
    expect(result).toEqual({ symbols: [], coordinates: [] });
  });
});

describe('buildFsw', () => {
  it('builds FSW from symbol data', () => {
    const symbols = [
      { key: 'S10000', x: 490, y: 510 },
      { key: 'S20500', x: 510, y: 490 }
    ];
    
    const result = buildFsw(symbols);
    expect(result).toBe('S10000490x510S20500510x490');
  });

  it('pads coordinates correctly', () => {
    const symbols = [{ key: 'S10000', x: 5, y: 25 }];
    const result = buildFsw(symbols);
    expect(result).toBe('S10000005x025');
  });

  it('handles empty symbol array', () => {
    const result = buildFsw([]);
    expect(result).toBe('');
  });

  it('handles null input', () => {
    const result = buildFsw(null);
    expect(result).toBe('');
  });

  it('handles non-array input', () => {
    const result = buildFsw('not-array');
    expect(result).toBe('');
  });
});
