import { describe, it, expect } from 'vitest';
import { createHistory, undo, redo, record } from './history';

describe('history pure functions', () => {
  it('creates initial history', () => {
    const h = createHistory('a');
    expect(h).toEqual({ past: [], present: 'a', future: [] });
  });

  it('records new present', () => {
    let h = createHistory('a');
    h = record(h, 'b');
    expect(h.past).toEqual(['a']);
    expect(h.present).toBe('b');
    expect(h.future).toEqual([]);
  });

  it('undoes to previous state', () => {
    let h = createHistory('a');
    h = record(h, 'b');
    h = record(h, 'c');
    h = undo(h);
    expect(h.present).toBe('b');
    expect(h.future[0]).toBe('c');
  });

  it('does not undo if no past', () => {
    let h = createHistory('a');
    h = undo(h);
    expect(h.present).toBe('a');
  });

  it('redoes to next state', () => {
    let h = createHistory('a');
    h = record(h, 'b');
    h = record(h, 'c');
    h = undo(h);
    h = redo(h);
    expect(h.present).toBe('c');
  });

  it('does not redo if no future', () => {
    let h = createHistory('a');
    h = redo(h);
    expect(h.present).toBe('a');
  });

  it('does not record if present is unchanged', () => {
    let h = createHistory('a');
    const h2 = record(h, 'a');
    expect(h2).toBe(h);
  });

  // Add a dummy test to ensure Vitest recognizes the suite
  it('dummy test', () => {
    expect(true).toBe(true);
  });
});
