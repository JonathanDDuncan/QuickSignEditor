/**
 * @fileoverview Svelte store for sign data management
 * Integrates with pure functions from utils for validation and processing
 */

import { writable, derived, get } from 'svelte/store';
import { validateSignData } from '../utils/validation';
import { historyReducer, initialHistoryState } from '../utils/history';
import { parseFSW, formatFSW } from '../utils/fsw';

/**
 * Main sign data store
 */
function createSignStore() {
  const { subscribe, set, update } = writable({
    name: '',
    symbols: [],
    fsw: 'M500x500',
    dimensions: { width: 500, height: 500 },
    metadata: {
      width: 400,
      height: 300,
      created: null,
      modified: null
    }
  });

  // Function to update FSW when symbols change
  const updateWithFSW = (data) => {
    const fswString = formatFSW(data.symbols);
    return { ...data, fsw: fswString || 'M500x500' };
  };

  return {
    subscribe,
    set: (data) => {
      set(updateWithFSW(data));
    },
    update: (updater) => {
      update(data => {
        const newData = updater(data);
        return updateWithFSW(newData);
      });
    },
    
    /**
     * Set complete sign data
     * @param {Object} data - Sign data object
     */
    setSignData: (data) => {
      const timestamp = Date.now();
      set({
        ...data,
        metadata: {
          ...data.metadata,
          modified: timestamp,
          created: data.metadata?.created || timestamp
        }
      });
    },

    /**
     * Update sign name
     * @param {string} name - New sign name
     */
    setName: (name) => update(data => ({ ...data, name })),

    /**
     * Update symbols array
     * @param {Array} symbols - New symbols array
     */
    setSymbols: (symbols) => update(data => ({ 
      ...data, 
      symbols,
      metadata: { ...data.metadata, modified: Date.now() }
    })),

    /**
     * Add a symbol to the sign
     * @param {Object} symbol - Symbol to add
     */
    addSymbol: (symbol) => update(data => ({
      ...data,
      symbols: [...data.symbols, symbol],
      metadata: { ...data.metadata, modified: Date.now() }
    })),    /**
     * Load sign data from FSW string
     * @param {string} fswString - FSW string to parse
     */
    loadFromFSW: (fswString) => {
      try {
        const parsed = parseFSW(fswString);
        const symbols = parsed.symbols.map((symbolKey, index) => ({
          id: `sym${index}`,
          symbol: symbolKey,
          x: parsed.coordinates[index]?.x || 100,
          y: parsed.coordinates[index]?.y || 100,
          selected: false
        }));
        
        const dimensions = { width: 500, height: 500 };
        
        set({
          name: '',
          symbols,
          fsw: fswString,
          dimensions,
          metadata: {
            width: dimensions.width,
            height: dimensions.height,
            created: Date.now(),
            modified: Date.now()
          }
        });
        
        // Clear history when loading new data
        historyStore.clear();
      } catch (error) {
        throw new Error('Invalid FSW format');
      }
    },

    /**
     * Undo last action
     */
    undo: () => {
      const previousState = historyStore.undo();
      if (previousState) {
        set(previousState);
      }
    },

    /**
     * Redo last undone action
     */
    redo: () => {
      // Redo functionality would be implemented with history store
      console.log('Redo functionality not yet implemented');
    },

    /**
     * Check if undo is available
     * @returns {boolean}
     */
    canUndo: () => {
      return historyStore.canUndo();
    },    /**
     * Check if redo is available
     * @returns {boolean}
     */
    canRedo: () => {
      return false; // Redo not implemented yet
    },

    /**
     * Select all symbols
     */
    selectAll: () => {
      update(data => ({
        ...data,
        symbols: data.symbols.map(symbol => ({ ...symbol, selected: true }))
      }));
    },

    /**
     * Clear all symbol selections
     */
    clearSelection: () => {
      update(data => ({
        ...data,
        symbols: data.symbols.map(symbol => ({ ...symbol, selected: false }))
      }));
    },

    /**
     * Delete selected symbols
     */
    deleteSelected: () => {
      update(data => ({
        ...data,
        symbols: data.symbols.filter(symbol => !symbol.selected)
      }));
    },

    /**
     * Clear all data
     */
    clear: () => set({
      name: '',
      symbols: [],
      fsw: 'M500x500',
      dimensions: { width: 500, height: 500 },
      metadata: { width: 400, height: 300, created: null, modified: null }
    })
  };
}

/**
 * History store for undo/redo functionality
 */
function createHistoryStore() {
  const { subscribe, set, update } = writable([]);

  return {
    subscribe,

    /**
     * Add current state to history
     * @param {Object} state - Current state to save
     * @param {string} actionName - Name of the action
     */
    addState: (state, actionName) => {
      update(history => addToUndoHistory(history, state, actionName));
    },

    /**
     * Undo last action
     * @returns {Object|null} Previous state or null
     */
    undo: () => {
      const currentHistory = get(historyStore);
      const { state, newHistory } = undoFromHistory(currentHistory);
      set(newHistory);
      return state;
    },

    /**
     * Check if undo is available
     * @returns {boolean} True if undo is available
     */
    canUndo: () => {
      const currentHistory = get(historyStore);
      return canUndo(currentHistory);
    },

    /**
     * Clear history
     */
    clear: () => set([])
  };
}

// Create store instances
export const signStore = createSignStore();
export const historyStore = createHistoryStore();

// Also export as signData for compatibility
export const signData = signStore;

// Derived store for validation
export const signValidation = derived(
  signStore,
  ($signData) => validateSignData($signData)
);

// Derived store for undo availability
export const canUndoStore = derived(
  historyStore,
  ($history) => canUndo($history)
);
