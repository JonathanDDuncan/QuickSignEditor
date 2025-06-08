/**
 * @fileoverview Tests for SymbolChooser.svelte component
 * Tests symbol library, categories, search, and responsive design
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { render, fireEvent, screen, waitFor } from '@testing-library/svelte';
import SymbolChooser from '../lib/components/SymbolChooser.svelte';
import { ui } from '../lib/stores';

// Mock the stores
vi.mock('../lib/stores', () => ({
  ui: {
    subscribe: vi.fn(),
    set: vi.fn(),
    update: vi.fn()
  }
}));

// Mock symbol data
const mockSymbolData = {
  hands: {
    name: 'Hands',
    symbols: [
      { symbol: 'S10000', name: 'Index finger' },
      { symbol: 'S10001', name: 'Fist' },
      { symbol: 'S10002', name: 'Flat hand' }
    ]
  },
  movement: {
    name: 'Movement',
    symbols: [
      { symbol: 'S20000', name: 'Straight movement' },
      { symbol: 'S20001', name: 'Curved movement' }
    ]
  },
  faces: {
    name: 'Faces',
    symbols: [
      { symbol: 'S30000', name: 'Happy face' },
      { symbol: 'S30001', name: 'Sad face' }
    ]
  }
};

describe('SymbolChooser Component', () => {
  let mockUiState = {
    selectedCategory: 'hands',
    searchTerm: '',
    showSymbolChooser: true
  };

  beforeEach(() => {
    // Reset mocks
    vi.clearAllMocks();
    
    // Setup store subscription
    ui.subscribe.mockImplementation((callback) => {
      callback(mockUiState);
      return () => {};
    });
  });

  describe('Basic Rendering', () => {
    it('renders the symbol chooser panel', () => {
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      expect(screen.getByRole('region', { name: /symbol chooser/i })).toBeInTheDocument();
    });

    it('shows category tabs', () => {
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      expect(screen.getByRole('tab', { name: /hands/i })).toBeInTheDocument();
      expect(screen.getByRole('tab', { name: /movement/i })).toBeInTheDocument();
      expect(screen.getByRole('tab', { name: /faces/i })).toBeInTheDocument();
    });

    it('highlights selected category tab', () => {
      mockUiState.selectedCategory = 'movement';
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const movementTab = screen.getByRole('tab', { name: /movement/i });
      expect(movementTab).toHaveAttribute('aria-selected', 'true');
    });

    it('shows search input', () => {
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      expect(screen.getByRole('searchbox', { name: /search symbols/i })).toBeInTheDocument();
    });
  });

  describe('Category Navigation', () => {
    it('switches categories when tab is clicked', async () => {
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const movementTab = screen.getByRole('tab', { name: /movement/i });
      await fireEvent.click(movementTab);
      
      expect(ui.update).toHaveBeenCalledWith(expect.any(Function));
    });

    it('shows symbols for selected category', () => {
      mockUiState.selectedCategory = 'hands';
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      expect(screen.getByText('Index finger')).toBeInTheDocument();
      expect(screen.getByText('Fist')).toBeInTheDocument();
      expect(screen.getByText('Flat hand')).toBeInTheDocument();
    });

    it('updates symbol display when category changes', () => {
      mockUiState.selectedCategory = 'movement';
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      expect(screen.getByText('Straight movement')).toBeInTheDocument();
      expect(screen.getByText('Curved movement')).toBeInTheDocument();
      expect(screen.queryByText('Index finger')).not.toBeInTheDocument();
    });
  });

  describe('Symbol Display', () => {
    it('renders symbol buttons with proper attributes', () => {
      mockUiState.selectedCategory = 'hands';
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const symbolButtons = screen.getAllByRole('button', { name: /drag to canvas/i });
      expect(symbolButtons).toHaveLength(3);
      
      symbolButtons.forEach(button => {
        expect(button).toHaveAttribute('draggable', 'true');
        expect(button).toHaveAttribute('data-symbol');
      });
    });

    it('shows symbol names as tooltips', () => {
      mockUiState.selectedCategory = 'hands';
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const indexFingerButton = screen.getByTitle('Index finger');
      expect(indexFingerButton).toBeInTheDocument();
    });

    it('renders symbol visual representation', () => {
      mockUiState.selectedCategory = 'hands';
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const symbolElements = screen.getAllByTestId(/symbol-visual/i);
      expect(symbolElements.length).toBeGreaterThan(0);
    });
  });

  describe('Search Functionality', () => {
    it('filters symbols based on search term', async () => {
      mockUiState.searchTerm = 'fist';
      mockUiState.selectedCategory = 'hands';
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      expect(screen.getByText('Fist')).toBeInTheDocument();
      expect(screen.queryByText('Index finger')).not.toBeInTheDocument();
      expect(screen.queryByText('Flat hand')).not.toBeInTheDocument();
    });

    it('shows no results message when search yields no matches', () => {
      mockUiState.searchTerm = 'nonexistent';
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      expect(screen.getByText(/no symbols found/i)).toBeInTheDocument();
    });

    it('searches across all categories when no category selected', () => {
      mockUiState.searchTerm = 'movement';
      mockUiState.selectedCategory = null;
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      expect(screen.getByText('Straight movement')).toBeInTheDocument();
      expect(screen.getByText('Curved movement')).toBeInTheDocument();
    });

    it('updates search term when typing in search box', async () => {
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const searchBox = screen.getByRole('searchbox', { name: /search symbols/i });
      await fireEvent.input(searchBox, { target: { value: 'hand' } });
      
      expect(ui.update).toHaveBeenCalledWith(expect.any(Function));
    });

    it('clears search when clear button is clicked', async () => {
      mockUiState.searchTerm = 'test';
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const clearButton = screen.getByRole('button', { name: /clear search/i });
      await fireEvent.click(clearButton);
      
      expect(ui.update).toHaveBeenCalledWith(expect.any(Function));
    });
  });

  describe('Drag and Drop', () => {
    it('sets drag data when symbol is dragged', async () => {
      mockUiState.selectedCategory = 'hands';
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const symbolButton = screen.getAllByRole('button', { name: /drag to canvas/i })[0];
      
      const dragEvent = new Event('dragstart', { bubbles: true });
      Object.defineProperty(dragEvent, 'dataTransfer', {
        value: {
          setData: vi.fn(),
          effectAllowed: ''
        }
      });
      
      await fireEvent(symbolButton, dragEvent);
      
      expect(dragEvent.dataTransfer.setData).toHaveBeenCalledWith('text/plain', 'S10000');
      expect(dragEvent.dataTransfer.effectAllowed).toBe('copy');
    });

    it('shows drag feedback during drag operation', async () => {
      mockUiState.selectedCategory = 'hands';
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const symbolButton = screen.getAllByRole('button', { name: /drag to canvas/i })[0];
      
      await fireEvent.dragStart(symbolButton);
      
      expect(symbolButton).toHaveClass('dragging');
    });

    it('removes drag feedback when drag ends', async () => {
      mockUiState.selectedCategory = 'hands';
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const symbolButton = screen.getAllByRole('button', { name: /drag to canvas/i })[0];
      
      await fireEvent.dragStart(symbolButton);
      await fireEvent.dragEnd(symbolButton);
      
      expect(symbolButton).not.toHaveClass('dragging');
    });
  });

  describe('Responsive Design', () => {
    it('adapts layout for mobile viewport', () => {
      // Mock window size
      Object.defineProperty(window, 'innerWidth', {
        writable: true,
        configurable: true,
        value: 375
      });
      
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const panel = screen.getByRole('region', { name: /symbol chooser/i });
      expect(panel).toHaveClass('mobile-layout');
    });

    it('shows grid layout for larger screens', () => {
      Object.defineProperty(window, 'innerWidth', {
        writable: true,
        configurable: true,
        value: 1024
      });
      
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const symbolGrid = screen.getByTestId('symbol-grid');
      expect(symbolGrid).toHaveClass('desktop-grid');
    });

    it('collapses categories on mobile when not selected', () => {
      Object.defineProperty(window, 'innerWidth', {
        writable: true,
        configurable: true,
        value: 375
      });
      
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const categoryTabs = screen.getByRole('tablist');
      expect(categoryTabs).toHaveClass('collapsed');
    });
  });

  describe('Keyboard Navigation', () => {
    it('allows tab navigation through categories', async () => {
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const firstTab = screen.getByRole('tab', { name: /hands/i });
      firstTab.focus();
      
      await fireEvent.keyDown(firstTab, { key: 'ArrowRight' });
      
      const secondTab = screen.getByRole('tab', { name: /movement/i });
      expect(secondTab).toHaveFocus();
    });

    it('allows arrow key navigation through symbols', async () => {
      mockUiState.selectedCategory = 'hands';
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const firstSymbol = screen.getAllByRole('button', { name: /drag to canvas/i })[0];
      firstSymbol.focus();
      
      await fireEvent.keyDown(firstSymbol, { key: 'ArrowRight' });
      
      const secondSymbol = screen.getAllByRole('button', { name: /drag to canvas/i })[1];
      expect(secondSymbol).toHaveFocus();
    });

    it('handles Enter key to add symbol to canvas', async () => {
      mockUiState.selectedCategory = 'hands';
      const { component } = render(SymbolChooser, { symbolData: mockSymbolData });
      
      const symbolButton = screen.getAllByRole('button', { name: /drag to canvas/i })[0];
      
      const addSymbolSpy = vi.fn();
      component.$on('addSymbol', addSymbolSpy);
      
      await fireEvent.keyDown(symbolButton, { key: 'Enter' });
      
      expect(addSymbolSpy).toHaveBeenCalledWith(
        expect.objectContaining({
          detail: { symbol: 'S10000' }
        })
      );
    });
  });

  describe('Accessibility', () => {
    it('has proper ARIA roles and labels', () => {
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      expect(screen.getByRole('region', { name: /symbol chooser/i })).toBeInTheDocument();
      expect(screen.getByRole('tablist')).toBeInTheDocument();
      expect(screen.getByRole('searchbox', { name: /search symbols/i })).toBeInTheDocument();
    });

    it('announces category changes to screen readers', async () => {
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const movementTab = screen.getByRole('tab', { name: /movement/i });
      await fireEvent.click(movementTab);
      
      const tabPanel = screen.getByRole('tabpanel');
      expect(tabPanel).toHaveAttribute('aria-labelledby');
    });

    it('provides appropriate symbol descriptions', () => {
      mockUiState.selectedCategory = 'hands';
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const symbolButtons = screen.getAllByRole('button', { name: /drag to canvas/i });
      symbolButtons.forEach(button => {
        expect(button).toHaveAttribute('aria-describedby');
      });
    });

    it('supports high contrast mode', () => {
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const panel = screen.getByRole('region', { name: /symbol chooser/i });
      expect(panel).toHaveClass('high-contrast-support');
    });
  });

  describe('Error Handling', () => {
    it('handles missing symbol data gracefully', () => {
      expect(() => render(SymbolChooser, { symbolData: {} })).not.toThrow();
    });

    it('handles invalid symbol data', () => {
      const invalidData = {
        invalid: {
          name: null,
          symbols: 'not an array'
        }
      };
      
      expect(() => render(SymbolChooser, { symbolData: invalidData })).not.toThrow();
    });

    it('shows error message when symbol loading fails', () => {
      render(SymbolChooser, { symbolData: null, error: 'Failed to load symbols' });
      
      expect(screen.getByText(/failed to load symbols/i)).toBeInTheDocument();
    });
  });

  describe('Performance', () => {
    it('virtualizes large symbol lists', () => {
      const largeSymbolData = {
        hands: {
          name: 'Hands',
          symbols: Array.from({ length: 1000 }, (_, i) => ({
            symbol: `S${10000 + i}`,
            name: `Symbol ${i}`
          }))
        }
      };
      
      mockUiState.selectedCategory = 'hands';
      render(SymbolChooser, { symbolData: largeSymbolData });
      
      // Should only render visible symbols
      const visibleSymbols = screen.getAllByRole('button', { name: /drag to canvas/i });
      expect(visibleSymbols.length).toBeLessThan(100);
    });

    it('debounces search input', async () => {
      render(SymbolChooser, { symbolData: mockSymbolData });
      
      const searchBox = screen.getByRole('searchbox', { name: /search symbols/i });
      
      // Type rapidly
      await fireEvent.input(searchBox, { target: { value: 'h' } });
      await fireEvent.input(searchBox, { target: { value: 'ha' } });
      await fireEvent.input(searchBox, { target: { value: 'han' } });
      await fireEvent.input(searchBox, { target: { value: 'hand' } });
      
      // Should debounce updates
      expect(ui.update).toHaveBeenCalledTimes(1);
    });
  });
});
