/**
 * @fileoverview Tests for ui.js store
 * Tests UI state management, tool selection, zoom, clipboard, and selections
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { get } from 'svelte/store';
import { ui } from '../lib/stores/ui.js';

describe('ui Store', () => {
  beforeEach(() => {
    // Reset store to initial state
    ui.set({
      selectedSymbol: null,
      tool: 'select',
      zoom: 1,
      showGrid: true,
      snapToGrid: true,
      showSymbolChooser: true,
      selectedCategory: 'hands',
      searchTerm: '',
      clipboard: null,
      showRulers: false,
      showGuides: true,
      theme: 'light'
    });
  });

  describe('Initial State', () => {
    it('has correct default values', () => {
      const state = get(ui);
      
      expect(state.selectedSymbol).toBeNull();
      expect(state.tool).toBe('select');
      expect(state.zoom).toBe(1);
      expect(state.showGrid).toBe(true);
      expect(state.snapToGrid).toBe(true);
      expect(state.showSymbolChooser).toBe(true);
      expect(state.selectedCategory).toBe('hands');
      expect(state.searchTerm).toBe('');
      expect(state.clipboard).toBeNull();
      expect(state.showRulers).toBe(false);
      expect(state.showGuides).toBe(true);
      expect(state.theme).toBe('light');
    });
  });

  describe('Tool Selection', () => {
    it('updates tool selection', () => {
      ui.update(state => ({ ...state, tool: 'hand' }));
      
      const state = get(ui);
      expect(state.tool).toBe('hand');
    });

    it('supports all tool types', () => {
      const tools = ['select', 'hand', 'zoom', 'pen'];
      
      tools.forEach(tool => {
        ui.update(state => ({ ...state, tool }));
        const state = get(ui);
        expect(state.tool).toBe(tool);
      });
    });

    it('resets selection when changing to non-select tool', () => {
      ui.update(state => ({ 
        ...state, 
        selectedSymbol: 'sym1',
        tool: 'hand'
      }));
      
      const state = get(ui);
      expect(state.selectedSymbol).toBeNull();
    });
  });

  describe('Symbol Selection', () => {
    it('selects a symbol', () => {
      ui.update(state => ({ ...state, selectedSymbol: 'sym1' }));
      
      const state = get(ui);
      expect(state.selectedSymbol).toBe('sym1');
    });

    it('clears selection', () => {
      ui.update(state => ({ ...state, selectedSymbol: 'sym1' }));
      ui.update(state => ({ ...state, selectedSymbol: null }));
      
      const state = get(ui);
      expect(state.selectedSymbol).toBeNull();
    });

    it('switches between symbols', () => {
      ui.update(state => ({ ...state, selectedSymbol: 'sym1' }));
      ui.update(state => ({ ...state, selectedSymbol: 'sym2' }));
      
      const state = get(ui);
      expect(state.selectedSymbol).toBe('sym2');
    });
  });

  describe('Zoom Control', () => {
    it('sets zoom level', () => {
      ui.update(state => ({ ...state, zoom: 1.5 }));
      
      const state = get(ui);
      expect(state.zoom).toBe(1.5);
    });

    it('constrains zoom to valid range', () => {
      ui.update(state => ({ ...state, zoom: 0.1 })); // Too small
      let state = get(ui);
      expect(state.zoom).toBeGreaterThanOrEqual(0.25);
      
      ui.update(state => ({ ...state, zoom: 10 })); // Too large
      state = get(ui);
      expect(state.zoom).toBeLessThanOrEqual(5);
    });

    it('supports zoom presets', () => {
      const presets = [0.25, 0.5, 1, 1.5, 2, 3, 4, 5];
      
      presets.forEach(zoom => {
        ui.update(state => ({ ...state, zoom }));
        const state = get(ui);
        expect(state.zoom).toBe(zoom);
      });
    });

    it('provides zoom methods', () => {
      // Zoom in
      ui.zoomIn();
      let state = get(ui);
      expect(state.zoom).toBeGreaterThan(1);
      
      // Zoom out
      const currentZoom = state.zoom;
      ui.zoomOut();
      state = get(ui);
      expect(state.zoom).toBeLessThan(currentZoom);
      
      // Reset zoom
      ui.resetZoom();
      state = get(ui);
      expect(state.zoom).toBe(1);
    });
  });

  describe('Grid and Snap Settings', () => {
    it('toggles grid visibility', () => {
      ui.update(state => ({ ...state, showGrid: false }));
      
      const state = get(ui);
      expect(state.showGrid).toBe(false);
    });

    it('toggles snap to grid', () => {
      ui.update(state => ({ ...state, snapToGrid: false }));
      
      const state = get(ui);
      expect(state.snapToGrid).toBe(false);
    });

    it('can show grid without snapping', () => {
      ui.update(state => ({ 
        ...state, 
        showGrid: true,
        snapToGrid: false
      }));
      
      const state = get(ui);
      expect(state.showGrid).toBe(true);
      expect(state.snapToGrid).toBe(false);
    });

    it('can snap without showing grid', () => {
      ui.update(state => ({ 
        ...state, 
        showGrid: false,
        snapToGrid: true
      }));
      
      const state = get(ui);
      expect(state.showGrid).toBe(false);
      expect(state.snapToGrid).toBe(true);
    });
  });

  describe('Symbol Chooser State', () => {
    it('toggles symbol chooser visibility', () => {
      ui.update(state => ({ ...state, showSymbolChooser: false }));
      
      const state = get(ui);
      expect(state.showSymbolChooser).toBe(false);
    });

    it('switches symbol categories', () => {
      ui.update(state => ({ ...state, selectedCategory: 'movement' }));
      
      const state = get(ui);
      expect(state.selectedCategory).toBe('movement');
    });

    it('supports all symbol categories', () => {
      const categories = ['hands', 'movement', 'faces', 'body', 'punctuation'];
      
      categories.forEach(category => {
        ui.update(state => ({ ...state, selectedCategory: category }));
        const state = get(ui);
        expect(state.selectedCategory).toBe(category);
      });
    });

    it('updates search term', () => {
      ui.update(state => ({ ...state, searchTerm: 'index' }));
      
      const state = get(ui);
      expect(state.searchTerm).toBe('index');
    });

    it('clears search term', () => {
      ui.update(state => ({ ...state, searchTerm: 'test' }));
      ui.update(state => ({ ...state, searchTerm: '' }));
      
      const state = get(ui);
      expect(state.searchTerm).toBe('');
    });
  });

  describe('Clipboard Management', () => {
    it('stores clipboard data', () => {
      const clipboardData = {
        symbol: 'S10000',
        x: 100,
        y: 100
      };
      
      ui.update(state => ({ ...state, clipboard: clipboardData }));
      
      const state = get(ui);
      expect(state.clipboard).toEqual(clipboardData);
    });

    it('clears clipboard', () => {
      ui.update(state => ({ 
        ...state, 
        clipboard: { symbol: 'S10000', x: 100, y: 100 }
      }));
      ui.update(state => ({ ...state, clipboard: null }));
      
      const state = get(ui);
      expect(state.clipboard).toBeNull();
    });

    it('provides clipboard utility methods', () => {
      expect(ui.hasClipboardData()).toBe(false);
      
      ui.update(state => ({ 
        ...state, 
        clipboard: { symbol: 'S10000', x: 100, y: 100 }
      }));
      
      expect(ui.hasClipboardData()).toBe(true);
      
      ui.clearClipboard();
      expect(ui.hasClipboardData()).toBe(false);
    });
  });

  describe('Display Settings', () => {
    it('toggles rulers', () => {
      ui.update(state => ({ ...state, showRulers: true }));
      
      const state = get(ui);
      expect(state.showRulers).toBe(true);
    });

    it('toggles guides', () => {
      ui.update(state => ({ ...state, showGuides: false }));
      
      const state = get(ui);
      expect(state.showGuides).toBe(false);
    });

    it('switches themes', () => {
      ui.update(state => ({ ...state, theme: 'dark' }));
      
      const state = get(ui);
      expect(state.theme).toBe('dark');
    });

    it('supports theme options', () => {
      const themes = ['light', 'dark', 'high-contrast'];
      
      themes.forEach(theme => {
        ui.update(state => ({ ...state, theme }));
        const state = get(ui);
        expect(state.theme).toBe(theme);
      });
    });
  });

  describe('Keyboard Shortcuts', () => {
    it('handles tool shortcuts', () => {
      ui.handleKeyboard({ key: 'v' }); // Select tool
      expect(get(ui).tool).toBe('select');
      
      ui.handleKeyboard({ key: 'h' }); // Hand tool
      expect(get(ui).tool).toBe('hand');
      
      ui.handleKeyboard({ key: 'z' }); // Zoom tool
      expect(get(ui).tool).toBe('zoom');
    });

    it('handles zoom shortcuts', () => {
      ui.handleKeyboard({ key: '=', ctrlKey: true }); // Zoom in
      expect(get(ui).zoom).toBeGreaterThan(1);
      
      ui.handleKeyboard({ key: '-', ctrlKey: true }); // Zoom out
      expect(get(ui).zoom).toBeLessThan(1.5);
      
      ui.handleKeyboard({ key: '0', ctrlKey: true }); // Reset zoom
      expect(get(ui).zoom).toBe(1);
    });

    it('handles grid shortcuts', () => {
      ui.handleKeyboard({ key: "'", ctrlKey: true }); // Toggle grid
      expect(get(ui).showGrid).toBe(false);
      
      ui.handleKeyboard({ key: ';', ctrlKey: true }); // Toggle snap
      expect(get(ui).snapToGrid).toBe(false);
    });
  });

  describe('State Persistence', () => {
    it('saves state to localStorage', () => {
      const mockLocalStorage = {
        setItem: vi.fn(),
        getItem: vi.fn(),
        removeItem: vi.fn()
      };
      
      Object.defineProperty(window, 'localStorage', {
        value: mockLocalStorage
      });
      
      ui.update(state => ({ ...state, zoom: 1.5, theme: 'dark' }));
      ui.savePreferences();
      
      expect(mockLocalStorage.setItem).toHaveBeenCalledWith(
        'quicksigneditor-preferences',
        expect.stringContaining('"zoom":1.5')
      );
    });

    it('loads state from localStorage', () => {
      const savedPreferences = {
        zoom: 2,
        theme: 'dark',
        showGrid: false,
        snapToGrid: false
      };
      
      const mockLocalStorage = {
        getItem: vi.fn(() => JSON.stringify(savedPreferences))
      };
      
      Object.defineProperty(window, 'localStorage', {
        value: mockLocalStorage
      });
      
      ui.loadPreferences();
      
      const state = get(ui);
      expect(state.zoom).toBe(2);
      expect(state.theme).toBe('dark');
      expect(state.showGrid).toBe(false);
      expect(state.snapToGrid).toBe(false);
    });

    it('handles invalid localStorage data', () => {
      const mockLocalStorage = {
        getItem: vi.fn(() => 'invalid-json')
      };
      
      Object.defineProperty(window, 'localStorage', {
        value: mockLocalStorage
      });
      
      expect(() => ui.loadPreferences()).not.toThrow();
    });
  });

  describe('Derived State', () => {
    it('calculates if symbol is selected', () => {
      ui.update(state => ({ ...state, selectedSymbol: 'sym1' }));
      
      expect(ui.isSymbolSelected('sym1')).toBe(true);
      expect(ui.isSymbolSelected('sym2')).toBe(false);
    });

    it('calculates if tool is active', () => {
      ui.update(state => ({ ...state, tool: 'hand' }));
      
      expect(ui.isToolActive('hand')).toBe(true);
      expect(ui.isToolActive('select')).toBe(false);
    });

    it('calculates editor mode', () => {
      // Select mode
      ui.update(state => ({ ...state, tool: 'select' }));
      expect(ui.getEditorMode()).toBe('select');
      
      // Pan mode
      ui.update(state => ({ ...state, tool: 'hand' }));
      expect(ui.getEditorMode()).toBe('pan');
      
      // Zoom mode
      ui.update(state => ({ ...state, tool: 'zoom' }));
      expect(ui.getEditorMode()).toBe('zoom');
    });
  });

  describe('Validation', () => {
    it('validates zoom values', () => {
      ui.update(state => ({ ...state, zoom: -1 }));
      expect(get(ui).zoom).toBeGreaterThan(0);
      
      ui.update(state => ({ ...state, zoom: 100 }));
      expect(get(ui).zoom).toBeLessThanOrEqual(5);
    });

    it('validates tool values', () => {
      ui.update(state => ({ ...state, tool: 'invalid-tool' }));
      expect(['select', 'hand', 'zoom', 'pen'].includes(get(ui).tool)).toBe(true);
    });

    it('validates theme values', () => {
      ui.update(state => ({ ...state, theme: 'invalid-theme' }));
      expect(['light', 'dark', 'high-contrast'].includes(get(ui).theme)).toBe(true);
    });
  });

  describe('Event Handling', () => {
    it('handles window resize', () => {
      const resizeEvent = new Event('resize');
      Object.defineProperty(window, 'innerWidth', { value: 800 });
      Object.defineProperty(window, 'innerHeight', { value: 600 });
      
      window.dispatchEvent(resizeEvent);
      
      // Should update layout calculations
      expect(ui.getViewportSize()).toEqual({ width: 800, height: 600 });
    });

    it('handles theme changes', () => {
      const mediaQuery = {
        matches: true,
        addEventListener: vi.fn(),
        removeEventListener: vi.fn()
      };
      
      window.matchMedia = vi.fn(() => mediaQuery);
      
      ui.watchSystemTheme();
      
      expect(window.matchMedia).toHaveBeenCalledWith('(prefers-color-scheme: dark)');
    });
  });

  describe('Performance', () => {
    it('debounces rapid updates', () => {
      const updates = [];
      const unsubscribe = ui.subscribe(state => updates.push(state));
      
      // Rapid zoom updates
      for (let i = 0; i < 10; i++) {
        ui.update(state => ({ ...state, zoom: 1 + i * 0.1 }));
      }
      
      // Should not create excessive updates
      expect(updates.length).toBeLessThan(15);
      
      unsubscribe();
    });

    it('batches related updates', () => {
      const batchedUpdate = {
        zoom: 1.5,
        showGrid: false,
        snapToGrid: false,
        theme: 'dark'
      };
      
      ui.update(state => ({ ...state, ...batchedUpdate }));
      
      const state = get(ui);
      expect(state.zoom).toBe(1.5);
      expect(state.showGrid).toBe(false);
      expect(state.snapToGrid).toBe(false);
      expect(state.theme).toBe('dark');
    });
  });

  describe('Error Handling', () => {
    it('handles invalid state updates gracefully', () => {
      expect(() => {
        ui.update(state => null); // Invalid update
      }).not.toThrow();
    });

    it('recovers from corrupted state', () => {
      // Simulate corrupted state
      ui.set(null);
      
      // Should reset to default state
      ui.reset();
      
      const state = get(ui);
      expect(state).toBeDefined();
      expect(state.tool).toBe('select');
      expect(state.zoom).toBe(1);
    });

    it('handles localStorage errors', () => {
      const mockLocalStorage = {
        setItem: vi.fn(() => { throw new Error('Storage full'); }),
        getItem: vi.fn(() => { throw new Error('Storage error'); })
      };
      
      Object.defineProperty(window, 'localStorage', {
        value: mockLocalStorage
      });
      
      expect(() => ui.savePreferences()).not.toThrow();
      expect(() => ui.loadPreferences()).not.toThrow();
    });
  });
});
