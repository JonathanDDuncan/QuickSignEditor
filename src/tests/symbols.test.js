/**
 * @fileoverview Tests for symbol manipulation functions
 * 100% coverage for all pure symbol functions
 */

import { describe, it, expect } from 'vitest';
import { 
  createSymbol,
  selectSymbol,
  deselectAllSymbols,
  getSelectedSymbols
} from '../lib/utils/symbols.js';

describe('createSymbol', () => {
  it('creates symbol with all parameters', () => {
    const result = createSymbol('S10000', 100, 200, 5);
    
    expect(result).toEqual({
      key: 'S10000',
      x: 100,
      y: 200,
      width: 30,
      height: 30,
      id: 5,
      selected: false,
      rotation: 0,
      fill: 1
    });
  });

  it('creates symbol with default values', () => {
    const result = createSymbol();
    
    expect(result).toEqual({
      key: '',
      x: 0,
      y: 0,
      width: 30,
      height: 30,
      id: 0,
      selected: false,
      rotation: 0,
      fill: 1
    });
  });
});

describe('selectSymbol', () => {
  it('selects symbol by ID', () => {
    const symbols = [
      { id: 1, selected: false },
      { id: 2, selected: false }
    ];
    
    const result = selectSymbol(symbols, 1, true);
    expect(result[0].selected).toBe(true);
    expect(result[1].selected).toBe(false);
  });

  it('deselects symbol by ID', () => {
    const symbols = [
      { id: 1, selected: true },
      { id: 2, selected: false }
    ];
    
    const result = selectSymbol(symbols, 1, false);
    expect(result[0].selected).toBe(false);
    expect(result[1].selected).toBe(false);
  });

  it('handles null symbols input', () => {
    const result = selectSymbol(null, 1, true);
    expect(result).toEqual([]);
  });

  it('handles non-array symbols input', () => {
    const result = selectSymbol('not-array', 1, true);
    expect(result).toEqual([]);
  });

  it('uses default selection value', () => {
    const symbols = [{ id: 1, selected: false }];
    const result = selectSymbol(symbols, 1);
    expect(result[0].selected).toBe(true);
  });
});

describe('deselectAllSymbols', () => {
  it('deselects all symbols', () => {
    const symbols = [
      { id: 1, selected: true },
      { id: 2, selected: true }
    ];
    
    const result = deselectAllSymbols(symbols);
    expect(result[0].selected).toBe(false);
    expect(result[1].selected).toBe(false);
  });

  it('handles already deselected symbols', () => {
    const symbols = [{ id: 1, selected: false }];
    const result = deselectAllSymbols(symbols);
    expect(result[0].selected).toBe(false);
  });

  it('handles null input', () => {
    const result = deselectAllSymbols(null);
    expect(result).toEqual([]);
  });

  it('handles non-array input', () => {
    const result = deselectAllSymbols('not-array');
    expect(result).toEqual([]);
  });
});

describe('getSelectedSymbols', () => {
  it('returns only selected symbols', () => {
    const symbols = [
      { id: 1, selected: true },
      { id: 2, selected: false },
      { id: 3, selected: true }
    ];
    
    const result = getSelectedSymbols(symbols);
    expect(result).toHaveLength(2);
    expect(result[0].id).toBe(1);
    expect(result[1].id).toBe(3);
  });

  it('returns empty array when no symbols selected', () => {
    const symbols = [
      { id: 1, selected: false },
      { id: 2, selected: false }
    ];
    
    const result = getSelectedSymbols(symbols);
    expect(result).toEqual([]);
  });

  it('handles null input', () => {
    const result = getSelectedSymbols(null);
    expect(result).toEqual([]);
  });

  it('handles non-array input', () => {
    const result = getSelectedSymbols('not-array');
    expect(result).toEqual([]);
  });
});
