/**
 * @fileoverview Svelte store for UI state management
 * Manages application UI state, selections, and user interactions
 */

import { writable, derived } from 'svelte/store';

/**
 * UI state store with default values matching test expectations
 */
const defaultUIState = {
  selectedSymbol: null,
  selectedSymbols: [],
  tool: 'select',
  zoom: 1,
  pan: { x: 0, y: 0 }, // Add pan property
  showGrid: true,
  snapToGrid: true,
  showSymbolChooser: true,
  selectedCategory: 'hands',
  searchTerm: '',
  clipboard: null,
  showRulers: false,
  showGuides: true,
  theme: 'light'
};

/**
 * Validates and constrains UI state values
 * @param {Object} state - UI state object
 * @returns {Object} Validated state
 */
function validateUIState(state) {
  // Handle null or invalid state
  if (!state || typeof state !== 'object') {
    return { ...defaultUIState };
  }
  
  const validTools = ['select', 'hand', 'zoom', 'pen'];
  const validThemes = ['light', 'dark', 'high-contrast'];
  
  return {
    ...defaultUIState,
    ...state,
    zoom: Math.max(0.25, Math.min(5, state.zoom ?? 1)),
    tool: validTools.includes(state.tool) ? state.tool : 'select',
    theme: validThemes.includes(state.theme) ? state.theme : 'light'
  };
}

/**
 * UI state store
 */
function createUIStore() {
  const { subscribe, set, update } = writable(defaultUIState);
  return {
    subscribe,
    set: (newState) => {
      const validatedState = validateUIState(newState);
      set(validatedState);
    },
    update: (updater) => {
      update(state => {
        try {
          const newState = updater(state);
          const validatedState = validateUIState(newState);
          
          // Handle tool-specific behavior - clear selection when switching away from select tool
          if (validatedState.tool !== 'select' && validatedState.tool !== state.tool) {
            validatedState.selectedSymbol = null;
          }
          
          return validatedState;
        } catch (error) {
          console.warn('Invalid state update, keeping current state:', error);
          return state;
        }
      });
    },

    /**
     * Set selected symbol
     * @param {string|null} symbol - Symbol to select
     */
    setSelectedSymbol: (symbol) => {
      update(state => ({ ...state, selectedSymbol: symbol }));
    },    /**
     * Set selected symbols array
     * @param {Array} symbols - Array of symbol IDs to select
     */
    setSelectedSymbols: (symbols) => {
      update(state => ({ ...state, selectedSymbols: Array.isArray(symbols) ? symbols : [] }));
    },

    /**
     * Add symbol to selection
     * @param {string} symbolId - Symbol ID to add to selection
     */
    addToSelection: (symbolId) => {
      update(state => ({
        ...state,
        selectedSymbols: state.selectedSymbols.includes(symbolId) 
          ? state.selectedSymbols 
          : [...state.selectedSymbols, symbolId]
      }));
    },

    /**
     * Remove symbol from selection
     * @param {string} symbolId - Symbol ID to remove from selection
     */
    removeFromSelection: (symbolId) => {
      update(state => ({
        ...state,
        selectedSymbols: state.selectedSymbols.filter(id => id !== symbolId)
      }));
    },

    /**
     * Clear all selections
     */
    clearSelection: () => {
      update(state => ({ ...state, selectedSymbol: null, selectedSymbols: [] }));
    },

    /**
     * Toggle symbol selection
     * @param {string} symbolId - Symbol ID to toggle
     */
    toggleSelection: (symbolId) => {
      update(state => ({
        ...state,
        selectedSymbols: state.selectedSymbols.includes(symbolId)
          ? state.selectedSymbols.filter(id => id !== symbolId)
          : [...state.selectedSymbols, symbolId]
      }));
    },

    /**
     * Set pan position
     * @param {Object} pan - Pan object with x and y properties
     */
    setPan: (pan) => {
      update(state => ({ ...state, pan: { x: pan.x || 0, y: pan.y || 0 } }));
    },

    /**
     * Set current tool
     * @param {string} tool - Tool name ('select', 'hand', 'zoom', etc.)
     */
    setTool: (tool) => {
      update(state => ({ ...state, tool }));
    },

    /**
     * Set zoom level
     * @param {number} zoom - Zoom level (0.25 to 4.0)
     */
    setZoom: (zoom) => {
      const clampedZoom = Math.max(0.25, Math.min(4.0, zoom));
      update(state => ({ ...state, zoom: clampedZoom }));
    },

    /**
     * Zoom in
     */
    zoomIn: () => {
      update(state => ({ ...state, zoom: Math.min(4.0, state.zoom * 1.25) }));
    },

    /**
     * Zoom out
     */
    zoomOut: () => {
      update(state => ({ ...state, zoom: Math.max(0.25, state.zoom / 1.25) }));
    },

    /**
     * Reset zoom to 100%
     */
    resetZoom: () => {
      update(state => ({ ...state, zoom: 1 }));
    },

    /**
     * Toggle grid visibility
     */
    toggleGrid: () => {
      update(state => ({ ...state, showGrid: !state.showGrid }));
    },

    /**
     * Toggle snap to grid
     */
    toggleSnapToGrid: () => {
      update(state => ({ ...state, snapToGrid: !state.snapToGrid }));
    },

    /**
     * Toggle symbol chooser visibility
     */
    toggleSymbolChooser: () => {
      update(state => ({ ...state, showSymbolChooser: !state.showSymbolChooser }));
    },

    /**
     * Set selected category
     * @param {string} category - Symbol category
     */
    setSelectedCategory: (category) => {
      update(state => ({ ...state, selectedCategory: category }));
    },

    /**
     * Set search term
     * @param {string} term - Search term
     */
    setSearchTerm: (term) => {
      update(state => ({ ...state, searchTerm: term }));
    },

    /**
     * Set clipboard content
     * @param {any} content - Content to copy to clipboard
     */
    setClipboard: (content) => {
      update(state => ({ ...state, clipboard: content }));
    },

    /**
     * Clear clipboard
     */
    clearClipboard: () => {
      update(state => ({ ...state, clipboard: null }));
    },

    /**
     * Toggle rulers visibility
     */
    toggleRulers: () => {
      update(state => ({ ...state, showRulers: !state.showRulers }));
    },

    /**
     * Toggle guides visibility
     */
    toggleGuides: () => {
      update(state => ({ ...state, showGuides: !state.showGuides }));
    },

    /**
     * Set theme
     * @param {string} theme - Theme name ('light' or 'dark')
     */
    setTheme: (theme) => {
      update(state => ({ ...state, theme }));
    },

    // Clipboard utility methods
    hasClipboardData: () => {
      let hasData = false;
      ui.subscribe(state => { hasData = state.clipboard !== null; })();
      return hasData;
    },

    // Keyboard shortcuts handler
    handleKeyboard: (event) => {
      const { key, ctrlKey, altKey, shiftKey } = event;
      
      if (ctrlKey) {
        switch (key) {
          case '=':
          case '+':
            update(state => ({ ...state, zoom: Math.min(5, state.zoom * 1.25) }));
            break;
          case '-':
            update(state => ({ ...state, zoom: Math.max(0.25, state.zoom / 1.25) }));
            break;
          case '0':
            update(state => ({ ...state, zoom: 1 }));
            break;          case "'":
            update(state => ({ ...state, showGrid: !state.showGrid }));
            break;
          case ';':
            update(state => ({ ...state, snapToGrid: !state.snapToGrid }));
            break;
        }
      } else {
        switch (key) {
          case 'v':
            update(state => ({ ...state, tool: 'select' }));
            break;
          case 'h':
            update(state => ({ ...state, tool: 'hand' }));
            break;
          case 'z':
            update(state => ({ ...state, tool: 'zoom' }));
            break;
          case 'p':
            update(state => ({ ...state, tool: 'pen' }));
            break;
        }
      }
    },

    // State persistence methods
    savePreferences: () => {
      try {
        let currentState = null;
        ui.subscribe(state => { currentState = state; })();
        
        const preferences = {
          zoom: currentState.zoom,
          theme: currentState.theme,
          showGrid: currentState.showGrid,
          snapToGrid: currentState.snapToGrid,
          showRulers: currentState.showRulers,
          showGuides: currentState.showGuides
        };
        
        localStorage.setItem('quicksigneditor-preferences', JSON.stringify(preferences));
      } catch (error) {
        console.warn('Failed to save preferences:', error);
      }
    },

    loadPreferences: () => {
      try {
        const saved = localStorage.getItem('quicksigneditor-preferences');
        if (saved) {
          const preferences = JSON.parse(saved);
          update(state => ({ ...state, ...preferences }));
        }
      } catch (error) {
        console.warn('Failed to load preferences:', error);
      }
    },

    // Derived state helpers
    isSymbolSelected: (symbolId) => {
      let isSelected = false;
      ui.subscribe(state => { isSelected = state.selectedSymbol === symbolId; })();
      return isSelected;
    },

    isToolActive: (toolName) => {
      let isActive = false;
      ui.subscribe(state => { isActive = state.tool === toolName; })();
      return isActive;
    },

    getEditorMode: () => {
      let mode = 'select';
      ui.subscribe(state => { 
        switch (state.tool) {
          case 'hand':
            mode = 'pan';
            break;
          case 'zoom':
            mode = 'zoom';
            break;
          case 'pen':
            mode = 'draw';
            break;
          default:
            mode = 'select';
        }
      })();
      return mode;
    },

    // Event handling
    getViewportSize: () => {
      return { width: 800, height: 600 }; // Default viewport size
    },

    watchSystemTheme: () => {
      if (typeof window !== 'undefined' && window.matchMedia) {
        const mediaQuery = window.matchMedia('(prefers-color-scheme: dark)');
        const handleThemeChange = (e) => {
          update(state => ({ ...state, theme: e.matches ? 'dark' : 'light' }));
        };
        
        mediaQuery.addEventListener('change', handleThemeChange);
        return () => mediaQuery.removeEventListener('change', handleThemeChange);
      }
    },

    // Reset to default state
    reset: () => {
      set(defaultUIState);
    }
  };
}

// Create store instance
export const ui = createUIStore();

// Derived stores for common UI queries
export const selectedSymbol = derived(
  ui,
  ($ui) => $ui.selectedSymbol
);

export const hasSelection = derived(
  ui,
  ($ui) => $ui.selectedSymbol !== null
);

export const currentTool = derived(
  ui,
  ($ui) => $ui.tool
);

export const zoomLevel = derived(
  ui,
  ($ui) => $ui.zoom
);

export const isSymbolChooserOpen = derived(
  ui,
  ($ui) => $ui.showSymbolChooser
);

export const hasClipboard = derived(
  ui,
  ($ui) => $ui.clipboard !== null
);

export const selectedSymbols = derived(
  ui,
  ($ui) => $ui.selectedSymbols
);

export const pan = derived(
  ui,
  ($ui) => $ui.pan
);
