/**
 * @fileoverview Integration tests for Svelte stores and components
 * Tests interaction between signData, ui stores and components
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { render, fireEvent, screen, waitFor } from '@testing-library/svelte';
import { get } from 'svelte/store';
import { signData, ui } from '../lib/stores';
import SignEditor from '../lib/components/SignEditor.svelte';
import SymbolChooser from '../lib/components/SymbolChooser.svelte';
import Toolbar from '../lib/components/Toolbar.svelte';

// Mock implementations for testing
vi.mock('$lib/utils/history', () => ({
  createHistoryManager: vi.fn(() => ({
    push: vi.fn(),
    undo: vi.fn(() => ({ symbols: [], fsw: 'M500x500', dimensions: { width: 500, height: 500 } })),
    redo: vi.fn(() => ({ symbols: [], fsw: 'M500x500', dimensions: { width: 500, height: 500 } })),
    canUndo: vi.fn(() => true),
    canRedo: vi.fn(() => false),
    clear: vi.fn()
  }))
}));

vi.mock('$lib/utils/fsw', () => {
  const buildFswMock = vi.fn(() => 'M500x500S10000100x100');
  const parseFswMock = vi.fn(() => ({
    symbols: [{ id: 'sym1', symbol: 'S10000', x: 100, y: 100 }],
    dimensions: { width: 500, height: 500 }
  }));
  const generateFSWMock = vi.fn(() => 'M500x500S10000100x100');
  return {
    buildFsw: buildFswMock,
    parseFsw: parseFswMock,
    generateFSW: generateFSWMock,
    formatFSW: buildFswMock,
    buildFSW: buildFswMock,
    parseFSW: parseFswMock
  };
});

vi.mock('$lib/utils/clipboard', () => ({
  copySymbol: vi.fn(() => ({ symbol: 'S10000', x: 100, y: 100 })),
  pasteSymbol: vi.fn(() => ({ id: 'new-sym', symbol: 'S10000', x: 150, y: 150 })),
  canPaste: vi.fn(() => true)
}));

vi.mock('$lib/utils/alignment', () => ({
  alignSymbols: vi.fn(),
  distributeSymbols: vi.fn()
}));

const mockSymbolData = {
  hands: {
    name: 'Hands',
    symbols: [
      { symbol: 'S10000', name: 'Index finger' },
      { symbol: 'S10001', name: 'Fist' }
    ]
  }
};

describe('Store and Component Integration', () => {
  beforeEach(() => {
    // Reset stores to initial state
    signData.set({
      symbols: [],
      fsw: 'M500x500',
      dimensions: { width: 500, height: 500 }
    });
    
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

  describe('SignEditor and Store Integration', () => {
    it('renders symbols from signData store', () => {
      signData.update((data: any) => ({
        ...data,
        symbols: [
          { id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: false },
          { id: 'sym2', symbol: 'S20000', x: 200, y: 200, selected: false }
        ]
      }));
      
      render(SignEditor);
      
      const symbols = screen.getAllByRole('button', { name: /symbol/i });
      expect(symbols).toHaveLength(2);
    });

    it('updates symbol selection in ui store when symbol is clicked', async () => {
      signData.update((data: any) => ({
        ...data,
        symbols: [
          { id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: false }
        ]
      }));
      
      render(SignEditor);
      
      const symbol = screen.getByRole('button', { name: /symbol/i });
      await fireEvent.click(symbol);
      
      await waitFor(() => {
        const uiState = get(ui);
        expect(uiState.selectedSymbol).toBe('sym1');
      });
    });

    it('updates symbol positions in signData store during drag', async () => {
      signData.update((data: any) => ({
        ...data,
        symbols: [
          { id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: true }
        ]
      }));
      
      const { container } = render(SignEditor);
      
      const symbol = screen.getByRole('button', { name: /symbol/i });
      
      await fireEvent.mouseDown(symbol, { clientX: 150, clientY: 150 });
      await fireEvent.mouseMove(container, { clientX: 200, clientY: 200 });
      await fireEvent.mouseUp(container);
      
      await waitFor(() => {
        const data = get(signData);
        expect(data.symbols[0].x).not.toBe(100);
        expect(data.symbols[0].y).not.toBe(100);
      });
    });

    it('reflects zoom changes from ui store', () => {
      ui.update((state: any) => ({ ...state, zoom: 1.5 }));
      
      render(SignEditor);
      
      const canvas = screen.getByRole('img', { name: /sign editor canvas/i });
      expect(canvas).toHaveStyle({ transform: 'scale(1.5)' });
    });

    it('shows/hides grid based on ui store settings', () => {
      ui.update((state: any) => ({ ...state, showGrid: false }));
      
      render(SignEditor);
      
      const canvas = screen.getByRole('img', { name: /sign editor canvas/i });
      expect(canvas).not.toHaveClass('show-grid');
    });
  });

  describe('SymbolChooser and Store Integration', () => {
    it('reflects selected category from ui store', () => {
      ui.update((state: any) => ({ ...state, selectedCategory: 'hands' }));
      
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const handsTab = screen.getByRole('tab', { name: /hands/i });
      expect(handsTab).toHaveAttribute('aria-selected', 'true');
    });

    it('updates ui store when category is changed', async () => {
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      // Add another category for testing
      const extendedSymbolData = {
        ...mockSymbolData,
        movement: { name: 'Movement', symbols: [{ symbol: 'S20000', name: 'Move' }] }
      };
      
      render(SymbolChooser, { symbolData: extendedSymbolData });
      
      const movementTab = screen.getByRole('tab', { name: /movement/i });
      await fireEvent.click(movementTab);
      
      await waitFor(() => {
        const uiState = get(ui);
        expect(uiState.selectedCategory).toBe('movement');
      });
    });

    it('filters symbols based on search term from ui store', () => {
      ui.update((state: any) => ({ ...state, searchTerm: 'fist' }));
      
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      expect(screen.getByText('Fist')).toBeInTheDocument();
      expect(screen.queryByText('Index finger')).not.toBeInTheDocument();
    });

    it('adds symbols to signData store when dropped on canvas', async () => {
      const { container } = render(SignEditor);
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const canvas = screen.getByRole('img', { name: /sign editor canvas/i });
      
      const dropEvent = new Event('drop', { bubbles: true });
      Object.defineProperty(dropEvent, 'dataTransfer', {
        value: { getData: vi.fn().mockReturnValue('S10000') }
      });
      Object.defineProperty(dropEvent, 'clientX', { value: 200 });
      Object.defineProperty(dropEvent, 'clientY', { value: 250 });
      
      await fireEvent(canvas, dropEvent);
      
      await waitFor(() => {
        const data = get(signData);
        expect(data.symbols.length).toBeGreaterThan(0);
      });
    });
  });

  describe('Toolbar and Store Integration', () => {
    it('enables/disables undo button based on signData history', () => {
      render(Toolbar);
      
      const undoButton = screen.getByRole('button', { name: /undo/i });
      expect(undoButton).toBeEnabled(); // Mock returns true
    });

    it('enables/disables clipboard buttons based on selection and clipboard state', () => {
      render(Toolbar);
      
      // No selection, no clipboard
      let copyButton = screen.getByRole('button', { name: /copy/i });
      let pasteButton = screen.getByRole('button', { name: /paste/i });
      expect(copyButton).toBeDisabled();
      expect(pasteButton).toBeEnabled(); // Mock returns true
      
      // With selection
      ui.update((state: any) => ({ ...state, selectedSymbol: 'sym1' }));
      render(Toolbar);
      
      copyButton = screen.getByRole('button', { name: /copy/i });
      expect(copyButton).toBeEnabled();
    });

    it('reflects active tool from ui store', () => {
      ui.update((state: any) => ({ ...state, tool: 'hand' }));
      
      render(Toolbar);
      
      const handToolButton = screen.getByRole('button', { name: /hand tool/i });
      expect(handToolButton).toHaveAttribute('aria-pressed', 'true');
    });

    it('updates tool in ui store when tool button is clicked', async () => {
      render(Toolbar);
      
      const zoomToolButton = screen.getByRole('button', { name: /zoom tool/i });
      await fireEvent.click(zoomToolButton);
      
      await waitFor(() => {
        const uiState = get(ui);
        expect(uiState.tool).toBe('zoom');
      });
    });
  });

  describe('Cross-Component Communication', () => {
    it('coordinates symbol selection across all components', async () => {
      // Setup initial data
      signData.update((data: any) => ({
        ...data,
        symbols: [
          { id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: false }
        ]
      }));
      
      const editorContainer = document.createElement('div');
      const toolbarContainer = document.createElement('div');
      
      render(SignEditor, { container: editorContainer });
      render(Toolbar, { container: toolbarContainer });
      
      // Select symbol in editor
      const symbol = screen.getByRole('button', { name: /symbol/i });
      await fireEvent.click(symbol);
      
      await waitFor(() => {
        // Check that ui store is updated
        const uiState = get(ui);
        expect(uiState.selectedSymbol).toBe('sym1');
        
        // Check that toolbar buttons are enabled
        const copyButton = screen.getByRole('button', { name: /copy/i });
        expect(copyButton).toBeEnabled();
      });
    });

    it('handles undo/redo operations across components', async () => {
      // Add some symbols first
      signData.update((data: any) => ({
        ...data,
        symbols: [
          { id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: false }
        ]
      }));
      
      render(SignEditor);
      render(Toolbar);
      
      // Perform undo
      const undoButton = screen.getByRole('button', { name: /undo/i });
      await fireEvent.click(undoButton);
      
      await waitFor(() => {
        // Check that signData is updated
        const data = get(signData);
        expect(data.symbols).toHaveLength(0);
      });
    });

    it('synchronizes clipboard operations', async () => {
      signData.update((data: any) => ({
        ...data,
        symbols: [
          { id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: false }
        ]
      }));
      
      ui.update((state: any) => ({ ...state, selectedSymbol: 'sym1' }));
      
      render(Toolbar);
      
      // Copy symbol
      const copyButton = screen.getByRole('button', { name: /copy/i });
      await fireEvent.click(copyButton);
      
      await waitFor(() => {
        const uiState = get(ui);
        expect(uiState.clipboard).toBeDefined();
      });
      
      // Paste symbol
      const pasteButton = screen.getByRole('button', { name: /paste/i });
      await fireEvent.click(pasteButton);
      
      await waitFor(() => {
        const data = get(signData);
        expect(data.symbols.length).toBeGreaterThan(1);
      });
    });
  });

  describe('State Consistency', () => {
    it('maintains consistent state during rapid operations', async () => {
      render(SignEditor);
      render(Toolbar);
      
      // Perform rapid operations
      for (let i = 0; i < 10; i++) {
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
      
      await waitFor(() => {
        const data = get(signData);
        expect(data.symbols).toHaveLength(10);
        expect(data.fsw).toContain('M500x500');
      });
    });

    it('handles store updates during component lifecycle', async () => {
      const { unmount } = render(SignEditor);
      
      // Update store after component is mounted
      signData.update((data: any) => ({
        ...data,
        symbols: [
          { id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: false }
        ]
      }));
      
      // Unmount and remount
      unmount();
      render(SignEditor);
      
      const symbols = screen.getAllByRole('button', { name: /symbol/i });
      expect(symbols).toHaveLength(1);
    });

    it('preserves ui state across component re-renders', () => {
      ui.update((state: any) => ({ 
        ...state, 
        zoom: 1.5,
        tool: 'hand',
        showGrid: false
      }));
      
      const { unmount } = render(SignEditor);
      unmount();
      
      render(SignEditor);
      
      const canvas = screen.getByRole('img', { name: /sign editor canvas/i });
      expect(canvas).toHaveStyle({ transform: 'scale(1.5)' });
      expect(canvas).not.toHaveClass('show-grid');
    });
  });

  describe('Error Recovery', () => {
    it('recovers gracefully from store errors', async () => {
      // Cause an error in signData
      signData.set(null);
      
      expect(() => render(SignEditor)).not.toThrow();
      expect(() => render(Toolbar)).not.toThrow();
    });

    it('maintains component functionality with partial store data', () => {
      // Set incomplete store data
      signData.set({
        symbols: [
          { id: 'sym1', x: 100, y: 100 } // Missing symbol and selected properties
        ],
        fsw: 'M500x500'
        // Missing dimensions
      });
      
      expect(() => render(SignEditor)).not.toThrow();
      
      const symbols = screen.getAllByRole('button', { name: /symbol/i });
      expect(symbols).toHaveLength(1);
    });

    it('handles concurrent store updates without conflicts', async () => {
      render(SignEditor);
      render(Toolbar);
      
      // Simulate concurrent updates from different sources
      const promises = [];
      
      for (let i = 0; i < 5; i++) {
        promises.push(
          signData.update((data: any) => ({
            ...data,
            symbols: [...data.symbols, {
              id: `sym${i}`,
              symbol: 'S10000',
              x: i * 30,
              y: i * 30,
              selected: false
            }]
          }))
        );
        
        promises.push(
          ui.update((state: any) => ({
            ...state,
            zoom: 1 + i * 0.1
          }))
        );
      }
      
      await Promise.all(promises);
      
      // Verify final state is consistent
      const data = get(signData);
      const uiState = get(ui);
      
      expect(data.symbols).toHaveLength(5);
      expect(uiState.zoom).toBeGreaterThan(1);
    });
  });
});
