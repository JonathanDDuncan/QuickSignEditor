/**
 * @fileoverview Tests for clipboard operations
 */

import { describe, test, expect } from 'vitest';
import { 
  copySelectedSymbols, 
  preparePasteSymbols, 
  alignSymbols 
} from '../lib/utils/clipboard.js';

describe('copySelectedSymbols', () => {
  test('copies selected symbols and deselects them', () => {
    const symbols = [
      { id: 1, x: 10, y: 20, selected: true },
      { id: 2, x: 30, y: 40, selected: false },
      { id: 3, x: 50, y: 60, selected: true }
    ];

    const result = copySelectedSymbols(symbols);

    expect(result).toHaveLength(2);
    expect(result[0]).toEqual({ id: 1, x: 10, y: 20, selected: false });
    expect(result[1]).toEqual({ id: 3, x: 50, y: 60, selected: false });
  });

  test('returns empty array for null input', () => {
    expect(copySelectedSymbols(null)).toEqual([]);
  });

  test('returns empty array for non-array input', () => {
    expect(copySelectedSymbols('not-array')).toEqual([]);
  });

  test('returns empty array when no symbols selected', () => {
    const symbols = [
      { id: 1, x: 10, y: 20, selected: false },
      { id: 2, x: 30, y: 40, selected: false }
    ];

    expect(copySelectedSymbols(symbols)).toEqual([]);
  });
});

describe('preparePasteSymbols', () => {
  test('prepares symbols for pasting at target position', () => {
    const clipboardSymbols = [
      { id: 1, x: 10, y: 20, width: 30, height: 40 },
      { id: 2, x: 50, y: 60, width: 70, height: 80 }
    ];
    const position = { x: 100, y: 200 };
    const nextId = 10;

    const result = preparePasteSymbols(clipboardSymbols, position, nextId);

    expect(result).toHaveLength(2);
    expect(result[0]).toEqual({ 
      id: 10, x: 100, y: 200, width: 30, height: 40, selected: true 
    });
    expect(result[1]).toEqual({ 
      id: 11, x: 140, y: 240, width: 70, height: 80, selected: true 
    });
  });  test('uses default position when not provided', () => {
    const clipboardSymbols = [{ id: 1, x: 10, y: 20 }];
    const nextId = 5;

    const result = preparePasteSymbols(clipboardSymbols, null, nextId);

    expect(result[0].x).toBe(0); // position.x (0) - firstSymbol.x (10) + symbol.x (10) = 0
    expect(result[0].y).toBe(0); // position.y (0) - firstSymbol.y (20) + symbol.y (20) = 0
  });

  test('returns empty array for null clipboard symbols', () => {
    expect(preparePasteSymbols(null, { x: 0, y: 0 }, 1)).toEqual([]);
  });

  test('returns empty array for empty clipboard symbols', () => {
    expect(preparePasteSymbols([], { x: 0, y: 0 }, 1)).toEqual([]);
  });
});

describe('alignSymbols', () => {
  const symbols = [
    { id: 1, x: 0, y: 0, width: 20, height: 20, selected: true },
    { id: 2, x: 100, y: 50, width: 30, height: 40, selected: true },
    { id: 3, x: 200, y: 100, width: 25, height: 35, selected: false }
  ];
  test('aligns symbols horizontally', () => {
    const result = alignSymbols(symbols, 'horizontal');
    
    // Bounds: minX=0, maxX=130, minY=0, maxY=90
    // centerY = 0 + (90-0)/2 = 45
    expect(result[0].y).toBe(35); // 45 - 20/2
    expect(result[1].y).toBe(25); // 45 - 40/2
    expect(result[2]).toEqual(symbols[2]); // Unselected, unchanged
  });

  test('aligns symbols vertically', () => {
    const result = alignSymbols(symbols, 'vertical');
    
    // Center X should be (0 + 130) / 2 = 65, adjusted for width
    expect(result[0].x).toBe(55); // 65 - 20/2
    expect(result[1].x).toBe(50); // 65 - 30/2
    expect(result[2]).toEqual(symbols[2]); // Unselected, unchanged
  });
  test('centers symbols both horizontally and vertically', () => {
    const result = alignSymbols(symbols, 'center');
    
    // centerX = 65, centerY = 45
    expect(result[0].x).toBe(55); // 65 - 20/2
    expect(result[0].y).toBe(35); // 45 - 20/2
    expect(result[1].x).toBe(50); // 65 - 30/2
    expect(result[1].y).toBe(25); // 45 - 40/2
    expect(result[2]).toEqual(symbols[2]); // Unselected, unchanged
  });

  test('returns original symbols for unknown alignment', () => {
    const result = alignSymbols(symbols, 'unknown');
    expect(result).toEqual(symbols);
  });

  test('returns original symbols when less than 2 selected', () => {
    const singleSelected = symbols.map((s, i) => ({ ...s, selected: i === 0 }));
    const result = alignSymbols(singleSelected, 'horizontal');
    expect(result).toEqual(singleSelected);
  });

  test('returns empty array for null input', () => {
    expect(alignSymbols(null, 'horizontal')).toEqual([]);
  });

  test('returns empty array for non-array input', () => {
    expect(alignSymbols('not-array', 'horizontal')).toEqual([]);
  });
});
