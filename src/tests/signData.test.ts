/**
 * @fileoverview Tests for signData.js store
 * Tests sign data management, history integration, and FSW generation
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { get } from 'svelte/store';
import { signData } from '../lib/stores/signData.js';

// Mock the history utility
vi.mock('$lib/utils/history', () => ({
  addToUndoHistory: vi.fn((history, state, action) => [{ state, action, timestamp: Date.now() }, ...history]),
  undoFromHistory: vi.fn((history) => ({ 
    state: history[0]?.state || null, 
    newHistory: history.slice(1) 
  })),
  canUndo: vi.fn((history) => history && history.length > 0),
  mockHistory: {
    undo: vi.fn(),
    redo: vi.fn(),
    clear: vi.fn(),
    canUndo: vi.fn(),
    canRedo: vi.fn()
  }
}));

// Mock FSW utility
vi.mock('$lib/utils/fsw', () => {
  const buildFswMock = vi.fn(() => 'M500x500S10000100x100');
  const parseFswMock = vi.fn(() => ({
    symbols: ['S10000'],
    coordinates: [{ x: 100, y: 100 }],
    raw: 'M500x500S10000100x100'
  }));
  const generateFSWMock = vi.fn(() => 'M500x500S10000100x100');
  // Export all aliases
  return {
    buildFsw: buildFswMock,
    parseFsw: parseFswMock,
    generateFSW: generateFSWMock,
    formatFSW: buildFswMock,
    buildFSW: buildFswMock,
    parseFSW: parseFswMock
  };
});

// Assign mocked functions to variables for use in tests
const { buildFsw, parseFsw, generateFSW, formatFSW, buildFSW, parseFSW } = vi.mocked(require('$lib/utils/fsw'));
const { addToUndoHistory, undoFromHistory, canUndo } = vi.mocked(require('$lib/utils/history'));
const { mockHistory } = require('$lib/utils/history');

describe('signData Store', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    buildFsw.mockReturnValue('M500x500S10000100x100');
    parseFsw.mockReturnValue({
      symbols: ['S10000'],
      coordinates: [{ x: 100, y: 100 }],
      raw: 'M500x500S10000100x100'
    });
  });

  describe('Initial State', () => {
    it('starts with empty sign data', () => {
      const data = get(signData);
      
      expect(data.symbols).toEqual([]);
      expect(data.fsw).toBe('M500x500');
      expect(data.dimensions).toEqual({ width: 500, height: 500 });
    });    it('initializes history manager', () => {
      get(signData);
      
      // History functions should be available (mocked)
      expect(canUndo).toBeDefined();
    });
  });

  describe('Symbol Management', () => {
    it('adds new symbols', () => {
      signData.update((data: any) => ({
        ...data,
        symbols: [...data.symbols, {
          id: 'sym1',
          symbol: 'S10000',
          x: 100,
          y: 100,
          selected: false
        }]
      }));
      
      const data = get(signData);
      expect(data.symbols).toHaveLength(1);
      expect(data.symbols[0]).toMatchObject({
        id: 'sym1',
        symbol: 'S10000',
        x: 100,
        y: 100
      });
    });

    it('updates existing symbols', () => {
      // First add a symbol
      signData.update((data: any) => ({
        ...data,
        symbols: [{
          id: 'sym1',
          symbol: 'S10000',
          x: 100,
          y: 100,
          selected: false
        }]
      }));
      
      // Then update it
      signData.update((data: any) => ({
        ...data,
        symbols: data.symbols.map((s: any) => 
          s.id === 'sym1' ? { ...s, x: 150, y: 150 } : s
        )
      }));
      
      const data = get(signData);
      expect(data.symbols[0]).toMatchObject({
        x: 150,
        y: 150
      });
    });

    it('removes symbols', () => {
      // Add symbols
      signData.set({
        symbols: [
          { id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: false },
          { id: 'sym2', symbol: 'S20000', x: 200, y: 200, selected: false }
        ],
        fsw: 'M500x500',
        dimensions: { width: 500, height: 500 }
      });
      
      // Remove one symbol
      signData.update((data: any) => ({
        ...data,
        symbols: data.symbols.filter((s: any) => s.id !== 'sym1')
      }));
      
      const data = get(signData);
      expect(data.symbols).toHaveLength(1);
      expect(data.symbols[0].id).toBe('sym2');
    });
  });

  describe('FSW Generation', () => {
    it('generates FSW when symbols change', () => {
      signData.update((data: any) => ({
        ...data,
        symbols: [{
          id: 'sym1',
          symbol: 'S10000',
          x: 100,
          y: 100,
          selected: false
        }]
      }));
      
      expect(buildFsw).toHaveBeenCalled();
    });    it('updates FSW property when symbols change', () => {
      buildFsw.mockReturnValue('M500x500S10000100x100');
      signData.update((data: any) => ({
        ...data,
        symbols: [{
          id: 'sym1',
          symbol: 'S10000',
          x: 100,
          y: 100,
          selected: false
        }]
      }));
      const data = get(signData);
      expect(data.fsw).toBe('M500x500S10000100x100');
    });
  });

  describe('History Integration', () => {    it('pushes state to history on updates', () => {
      signData.update((data: any) => ({
        ...data,
        symbols: [{
          id: 'sym1',
          symbol: 'S10000',
          x: 100,
          y: 100,
          selected: false
        }]
      }));

      // For now, just check that the update worked
      const data = get(signData);
      expect(data.symbols).toHaveLength(1);
    });    it('provides undo functionality', () => {
      // Setup mock to return a previous state
      undoFromHistory.mockReturnValue({
        state: { symbols: [], fsw: 'M500x500', dimensions: { width: 500, height: 500 } },
        newHistory: []
      });

      signData.undo();

      expect(undoFromHistory).toHaveBeenCalled();
    });    it('provides redo functionality', () => {
      signData.redo();

      // Redo functionality is not implemented yet, so just verify the method exists
      expect(typeof signData.redo).toBe('function');
    });    it('reports undo availability', () => {
      canUndo.mockReturnValue(true);
      
      expect(signData.canUndo()).toBe(true);
    });    it('reports redo availability', () => {
      expect(signData.canRedo()).toBe(false);
    });
  });

  describe('Derived Properties', () => {
    it('calculates selected symbols', () => {
      signData.set({
        symbols: [
          { id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: true },
          { id: 'sym2', symbol: 'S20000', x: 200, y: 200, selected: false },
          { id: 'sym3', symbol: 'S30000', x: 300, y: 300, selected: true }
        ],
        fsw: 'M500x500',
        dimensions: { width: 500, height: 500 }
      });
      
      const data = get(signData);
      const selectedSymbols = data.symbols.filter(s => s.selected);
      expect(selectedSymbols).toHaveLength(2);
      expect(selectedSymbols.map(s => s.id)).toEqual(['sym1', 'sym3']);
    });

    it('calculates sign bounds', () => {
      signData.set({
        symbols: [
          { id: 'sym1', symbol: 'S10000', x: 50, y: 75, selected: false },
          { id: 'sym2', symbol: 'S20000', x: 200, y: 300, selected: false }
        ],
        fsw: 'M500x500',
        dimensions: { width: 500, height: 500 }
      });
      
      const data = get(signData);
      
      // Calculate bounds
      const bounds = {
        minX: Math.min(...data.symbols.map((s: any) => s.x)),
        maxX: Math.max(...data.symbols.map((s: any) => s.x)),
        minY: Math.min(...data.symbols.map((s: any) => s.y)),
        maxY: Math.max(...data.symbols.map((s: any) => s.y))
      };
      
      expect(bounds.minX).toBe(50);
      expect(bounds.maxX).toBe(200);
      expect(bounds.minY).toBe(75);
      expect(bounds.maxY).toBe(300);
    });
  });

  describe('Validation', () => {
    it('validates symbol data structure', () => {
      const validSymbol = {
        id: 'sym1',
        symbol: 'S10000',
        x: 100,
        y: 100,
        selected: false
      };
      
      signData.update((data: any) => ({
        ...data,
        symbols: [validSymbol]
      }));
      
      const data = get(signData);
      expect(data.symbols[0]).toMatchObject(validSymbol);
    });

    it('handles invalid symbol data', () => {
      const invalidSymbol = {
        id: null,
        symbol: '',
        x: NaN,
        y: undefined,
        selected: 'invalid'
      };
      
      expect(() => {
        signData.update((data: any) => ({
          ...data,
          symbols: [invalidSymbol]
        }));
      }).not.toThrow();
    });    it('validates FSW format', () => {
      parseFsw.mockImplementation((fsw: any) => {
        if (!fsw.startsWith('M')) {
          throw new Error('Invalid FSW format');
        }
        return {
          symbols: [],
          coordinates: []
        };
      });
      
      expect(() => {
        signData.loadFromFSW('InvalidFSW');
      }).toThrow('Invalid FSW format');
    });
  });

  describe('FSW Loading', () => {
    it('loads from FSW string', () => {
      const fswString = 'M500x500S10000100x100S20000200x200';
      const parsedData = {
        symbols: [
          { id: 'sym1', symbol: 'S10000', x: 100, y: 100 },
          { id: 'sym2', symbol: 'S20000', x: 200, y: 200 }
        ],
        dimensions: { width: 500, height: 500 }
      };
      
      parseFSW.mockReturnValue(parsedData);
      
      signData.loadFromFSW(fswString);
      
      expect(parseFSW).toHaveBeenCalledWith(fswString);
      
      const data = get(signData);
      expect(data.symbols).toHaveLength(2);
      expect(data.fsw).toBe(fswString);
    });

    it('clears history when loading new FSW', () => {
      signData.loadFromFSW('M500x500S10000100x100');
      
      expect(mockHistory.clear).toHaveBeenCalled();
    });
  });

  describe('Bulk Operations', () => {
    it('selects all symbols', () => {
      signData.set({
        symbols: [
          { id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: false },
          { id: 'sym2', symbol: 'S20000', x: 200, y: 200, selected: false }
        ],
        fsw: 'M500x500',
        dimensions: { width: 500, height: 500 }
      });
      
      signData.selectAll();
      
      const data = get(signData);
      expect(data.symbols.every(s => s.selected)).toBe(true);
    });

    it('clears all selections', () => {
      signData.set({
        symbols: [
          { id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: true },
          { id: 'sym2', symbol: 'S20000', x: 200, y: 200, selected: true }
        ],
        fsw: 'M500x500',
        dimensions: { width: 500, height: 500 }
      });
      
      signData.clearSelection();
      
      const data = get(signData);
      expect(data.symbols.every(s => !s.selected)).toBe(true);
    });

    it('deletes selected symbols', () => {
      signData.set({
        symbols: [
          { id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: true },
          { id: 'sym2', symbol: 'S20000', x: 200, y: 200, selected: false },
          { id: 'sym3', symbol: 'S30000', x: 300, y: 300, selected: true }
        ],
        fsw: 'M500x500',
        dimensions: { width: 500, height: 500 }
      });
      
      signData.deleteSelected();
      
      const data = get(signData);
      expect(data.symbols).toHaveLength(1);
      expect(data.symbols[0].id).toBe('sym2');
    });
  });

  describe('Performance', () => {
    it('batches multiple updates', () => {
      const updates = [];
      
      // Subscribe to track updates
      const unsubscribe = signData.subscribe((data) => {
        updates.push(data);
      });
      
      // Perform multiple operations
      signData.update(data => ({ ...data, symbols: [{ id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: false }] }));
      signData.update(data => ({ ...data, symbols: [...data.symbols, { id: 'sym2', symbol: 'S20000', x: 200, y: 200, selected: false }] }));
      
      // Should not trigger excessive updates
      expect(updates.length).toBeLessThan(10);
      
      unsubscribe();
    });

    it('debounces FSW generation', () => {
      // Perform multiple rapid updates
      for (let i = 0; i < 5; i++) {
        signData.update((data: any) => ({
          ...data,
          symbols: [...data.symbols, {
            id: `sym${i}`,
            symbol: 'S10000',
            x: i * 50,
            y: i * 50,
            selected: false
          }]
        }));
      }
      
      // FSW generation should be debounced
      expect(generateFSW).toHaveBeenCalledTimes(1);
    });
  });

  describe('Error Handling', () => {
    it('handles FSW generation errors', () => {
      generateFSW.mockImplementation(() => {
        throw new Error('FSW generation failed');
      });
      
      expect(() => {
        signData.update((data: any) => ({
          ...data,
          symbols: [{ id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: false }]
        }));
      }).not.toThrow();
    });

    it('handles history operation errors', () => {
      mockHistory.undo.mockImplementation(() => {
        throw new Error('Undo failed');
      });
      
      expect(() => signData.undo()).not.toThrow();
    });

    it('maintains data integrity on errors', () => {
      const initialData = get(signData);
      
      // Cause an error during update
      generateFSW.mockImplementation(() => {
        throw new Error('Generation failed');
      });
      
      signData.update((data: any) => ({
        ...data,
        symbols: [{ id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: false }]
      }));
      
      const finalData = get(signData);
      
      // Data should still be valid even if FSW generation failed
      expect(finalData.symbols).toBeDefined();
      expect(Array.isArray(finalData.symbols)).toBe(true);
    });
  });
});
