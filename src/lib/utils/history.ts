import type { Symbol, History } from './types';

// Pure function: createHistory
// Returns a new history object with initial state
export function createHistory<T>(initial: T): { past: T[]; present: T; future: T[] } {
  return {
    past: [],
    present: initial,
    future: []
  };
}

// Pure function: undo
// Moves present to future, pops past to present
export function undo<T>(history: { past: T[]; present: T; future: T[] }): { past: T[]; present: T; future: T[] } {
  if (history.past.length === 0) return history;
  const previous = history.past[history.past.length - 1];
  return {
    past: history.past.slice(0, -1),
    present: previous,
    future: [history.present, ...history.future]
  };
}

// Pure function: redo
// Moves present to past, shifts future to present
export function redo<T>(history: { past: T[]; present: T; future: T[] }): { past: T[]; present: T; future: T[] } {
  if (history.future.length === 0) return history;
  const next = history.future[0];
  return {
    past: [...history.past, history.present],
    present: next,
    future: history.future.slice(1)
  };
}

// Pure function: record
// Pushes present to past, sets new present, clears future
export function record<T>(history: { past: T[]; present: T; future: T[] }, newPresent: T): { past: T[]; present: T; future: T[] } {
  if (history.present === newPresent) return history;
  return {
    past: [...history.past, history.present],
    present: newPresent,
    future: []
  };
}

// For Vitest compatibility, also export as default
export default { createHistory, undo, redo, record };
