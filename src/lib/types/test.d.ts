// Test helper types for Vitest and Svelte Testing Library

import type { SignData, UIState, SymbolData } from './index';

export interface MockStore<T> {
  subscribe: (callback: (value: T) => void) => () => void;
  update: (updater: (current: T) => T) => void;
  set: (value: T) => void;
}
