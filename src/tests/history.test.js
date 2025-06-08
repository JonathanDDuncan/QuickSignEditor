/**
 * @fileoverview Tests for history management functions
 */

import { describe, test, expect, vi } from 'vitest';
import { 
  addToUndoHistory, 
  undoFromHistory, 
  canUndo 
} from '../lib/utils/history.js';

describe('addToUndoHistory', () => {
  test('adds state to empty history', () => {
    const state = { symbols: [], currentTool: 'select' };
    const result = addToUndoHistory([], state, 'test-action');

    expect(result).toHaveLength(1);
    expect(result[0].action).toBe('test-action');
    expect(result[0].state).toEqual(state);
    expect(result[0].timestamp).toBeDefined();
  });

  test('adds state to existing history', () => {
    const existingHistory = [
      { action: 'old-action', state: { test: 1 }, timestamp: 123456 }
    ];
    const newState = { symbols: [{ id: 1 }], currentTool: 'move' };
    
    const result = addToUndoHistory(existingHistory, newState, 'new-action');

    expect(result).toHaveLength(2);
    expect(result[0].action).toBe('new-action');
    expect(result[0].state).toEqual(newState);
    expect(result[1]).toEqual(existingHistory[0]);
  });

  test('creates deep copy of state', () => {
    const state = { symbols: [{ id: 1, x: 10 }] };
    const result = addToUndoHistory([], state, 'test');

    // Modify original state
    state.symbols[0].x = 20;

    // History should be unchanged
    expect(result[0].state.symbols[0].x).toBe(10);
  });
  test('respects maximum history limit', () => {
    const largeHistory = Array.from({ length: 60 }, (_, i) => ({
      action: `action-${i}`,
      state: { step: i },
      timestamp: i
    }));

    const result = addToUndoHistory(largeHistory, { new: true }, 'new-action', 50);

    expect(result).toHaveLength(50);
    expect(result[0].action).toBe('new-action');
    expect(result[49].action).toBe('action-48'); // Should keep newest 49 from original: indices 0-48
  });

  test('handles null history input', () => {
    const state = { test: true };
    const result = addToUndoHistory(null, state, 'test-action');

    expect(result).toHaveLength(1);
    expect(result[0].state).toEqual(state);
  });

  test('handles non-array history input', () => {
    const state = { test: true };
    const result = addToUndoHistory('not-array', state, 'test-action');

    expect(result).toHaveLength(1);
    expect(result[0].state).toEqual(state);
  });

  test('uses default maxHistory of 50', () => {
    // Create history with 55 items
    const largeHistory = Array.from({ length: 55 }, (_, i) => ({
      action: `action-${i}`,
      state: { step: i },
      timestamp: i
    }));

    const result = addToUndoHistory(largeHistory, { new: true }, 'new-action');

    expect(result).toHaveLength(50);
  });
});

describe('undoFromHistory', () => {
  test('returns previous state and removes it from history', () => {
    const history = [
      { action: 'recent', state: { step: 2 }, timestamp: 200 },
      { action: 'older', state: { step: 1 }, timestamp: 100 }
    ];

    const result = undoFromHistory(history);

    expect(result.state).toEqual({ step: 2 });
    expect(result.newHistory).toHaveLength(1);
    expect(result.newHistory[0].action).toBe('older');
  });

  test('returns null state for empty history', () => {
    const result = undoFromHistory([]);

    expect(result.state).toBeNull();
    expect(result.newHistory).toEqual([]);
  });

  test('handles null history input', () => {
    const result = undoFromHistory(null);

    expect(result.state).toBeNull();
    expect(result.newHistory).toEqual([]);
  });

  test('returns last remaining item', () => {
    const history = [
      { action: 'last', state: { final: true }, timestamp: 100 }
    ];

    const result = undoFromHistory(history);

    expect(result.state).toEqual({ final: true });
    expect(result.newHistory).toEqual([]);
  });
});

describe('canUndo', () => {
  test('returns true for non-empty history', () => {
    const history = [
      { action: 'test', state: {}, timestamp: 123 }
    ];

    expect(canUndo(history)).toBe(true);
  });

  test('returns false for empty history', () => {
    expect(canUndo([])).toBe(false);
  });

  test('returns false for null history', () => {
    expect(canUndo(null)).toBe(false);
  });

  test('returns false for undefined history', () => {
    expect(canUndo(undefined)).toBe(false);
  });

  test('returns false for non-array history', () => {
    expect(canUndo('not-array')).toBe(false);
    expect(canUndo({})).toBe(false);
    expect(canUndo(123)).toBe(false);
  });

  test('returns true for history with multiple items', () => {
    const history = [
      { action: 'action1', state: {}, timestamp: 100 },
      { action: 'action2', state: {}, timestamp: 200 },
      { action: 'action3', state: {}, timestamp: 300 }
    ];

    expect(canUndo(history)).toBe(true);
  });
});
