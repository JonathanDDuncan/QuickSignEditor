/**
 * @fileoverview Tests for Toolbar.svelte component
 * Tests action controls, undo/redo, copy/paste, alignment tools
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { render, fireEvent, screen } from '@testing-library/svelte';
import Toolbar from '../lib/components/Toolbar.svelte';
import { signData, ui } from '../lib/stores';

// Mock the stores
vi.mock('../lib/stores', () => {
  // Helper to create a Svelte-like store with a mockable subscribe
  const makeMockStore = (value: any) => {
    const store = {
      subscribe: vi.fn((cb: any) => { cb(value); return () => {}; }),
      set: vi.fn(),
      update: vi.fn()
    };
    return store;
  };
  // Default UI state for tests
  const mockUiState: any = {
    selectedSymbol: 'sym1',
    selectedSymbols: ['sym1'],
    tool: 'select',
    zoom: 1,
    pan: { x: 0, y: 0 },
    clipboard: null,
    showGrid: true,
    snapToGrid: true,
    showSymbolChooser: true,
    selectedCategory: 'hands',
    searchTerm: '',
    showRulers: false,
    showGuides: true,
    theme: 'light'
  };
  // Svelte-like store for ui
  const ui: any = {
    subscribe: vi.fn((cb: any) => { cb(mockUiState); return () => {}; }),
    set: vi.fn(),
    update: vi.fn(),
    // UI methods
    setClipboard: vi.fn(),
    setSelectedSymbols: vi.fn(),
    clearSelection: vi.fn(),
    setTool: vi.fn(),
    setCurrentTool: vi.fn(),
    setZoom: vi.fn(),
    toggleGrid: vi.fn(),
    toggleSnapToGrid: vi.fn(),
    toggleSymbolChooser: vi.fn(),
    setSelectedCategory: vi.fn(),
    setSearchTerm: vi.fn(),
    clearClipboard: vi.fn(),
    setPan: vi.fn(),
    setTheme: vi.fn(),
    zoomIn: vi.fn(),
    zoomOut: vi.fn(),
    resetZoom: vi.fn(),
    toggleRulers: vi.fn(),
    toggleGuides: vi.fn(),
    savePreferences: vi.fn(),
    loadPreferences: vi.fn(),
    handleKeyboard: vi.fn(),
    getEditorMode: vi.fn(() => 'select'),
    getViewportSize: vi.fn(() => ({ width: 800, height: 600 })),
    watchSystemTheme: vi.fn(),
    reset: vi.fn(),
    // Clipboard utility
    hasClipboardData: vi.fn(() => !!mockUiState.clipboard),
    // Derived state helpers
    isSymbolSelected: vi.fn((id: any) => mockUiState.selectedSymbol === id),
    isToolActive: vi.fn((tool: any) => mockUiState.tool === tool),
    // Spread state
    ...mockUiState
  };
  // Attach derived Svelte-like stores as properties (cast as any)
  ui.selectedSymbol = makeMockStore(mockUiState.selectedSymbol) as any;
  ui.selectedSymbols = makeMockStore(mockUiState.selectedSymbols) as any;
  ui.tool = makeMockStore(mockUiState.tool) as any;
  ui.clipboard = makeMockStore(mockUiState.clipboard) as any;
  ui.currentTool = makeMockStore(mockUiState.tool) as any;
  ui.hasClipboard = makeMockStore(!!mockUiState.clipboard) as any;
  ui.hasSelection = makeMockStore(!!mockUiState.selectedSymbol) as any;
  ui.zoomLevel = makeMockStore(mockUiState.zoom) as any;
  ui.isSymbolChooserOpen = makeMockStore(mockUiState.showSymbolChooser) as any;
  ui.pan = makeMockStore(mockUiState.pan) as any;
  // Export all as top-level too
  return {
    signData: {
      subscribe: vi.fn(),
      set: vi.fn(),
      update: vi.fn(),
      undo: vi.fn(),
      redo: vi.fn(),
      canUndo: vi.fn(),
      canRedo: vi.fn(),
      setSymbols: vi.fn(),
      setSignData: vi.fn()
    },
    ui,
    selectedSymbol: ui.selectedSymbol,
    selectedSymbols: ui.selectedSymbols,
    tool: ui.tool,
    clipboard: ui.clipboard,
    currentTool: ui.currentTool,
    hasClipboard: ui.hasClipboard,
    hasSelection: ui.hasSelection,
    zoomLevel: ui.zoomLevel,
    isSymbolChooserOpen: ui.isSymbolChooserOpen,
    pan: ui.pan
  };
});

// Mock utility functions
vi.mock('$lib/utils/clipboard', () => ({
  copySymbol: vi.fn(),
  pasteSymbol: vi.fn(),
  canPaste: vi.fn()
}));
vi.mock('$lib/utils/alignment', () => ({
  alignSymbols: vi.fn(),
  distributeSymbols: vi.fn()
}));

import { copySymbol, pasteSymbol, canPaste } from '$lib/utils/clipboard';
import { alignSymbols, distributeSymbols } from '$lib/utils/alignment';

describe('Toolbar Component', () => {
  let mockSignData = {
    symbols: [
      {
        id: 'sym1',
        symbol: 'S10000',
        x: 100,
        y: 100,
        selected: true
      },
      {
        id: 'sym2',
        symbol: 'S20000',
        x: 200,
        y: 150,
        selected: false
      }
    ],
    fsw: 'M500x500S10000100x100S20000200x150'
  };
  
  let mockUiState = {
    selectedSymbol: 'sym1',
    tool: 'select',
    clipboard: null
  };

  beforeEach(() => {
    // Reset mocks
    vi.clearAllMocks();
    // Setup store subscriptions
    (signData as any).subscribe.mockImplementation((callback: any) => {
      callback(mockSignData as any);
      return () => {};
    });
    (ui as any).subscribe.mockImplementation((callback: any) => {
      callback(mockUiState as any);
      return () => {};
    });
    (ui as any).selectedSymbol.subscribe.mockImplementation((callback: any) => {
      callback(mockUiState.selectedSymbol as any);
      return () => {};
    });
    (ui as any).tool.subscribe.mockImplementation((callback: any) => {
      callback(mockUiState.tool as any);
      return () => {};
    });
    (ui as any).clipboard.subscribe.mockImplementation((callback: any) => {
      callback(mockUiState.clipboard as any);
      return () => {};
    });
    // Setup utility function defaults
    (signData as any).canUndo.mockReturnValue(true);
    (signData as any).canRedo.mockReturnValue(false);
    (canPaste as any).mockReturnValue(false);
  });

  describe('Basic Rendering', () => {
    it('renders the toolbar', () => {
      render(Toolbar);
      
      expect(screen.getByRole('toolbar', { name: /main toolbar/i })).toBeInTheDocument();
    });

    it('shows all tool groups', () => {
      render(Toolbar);
      
      expect(screen.getByRole('group', { name: /history/i })).toBeInTheDocument();
      expect(screen.getByRole('group', { name: /clipboard/i })).toBeInTheDocument();
      expect(screen.getByRole('group', { name: /alignment/i })).toBeInTheDocument();
      expect(screen.getByRole('group', { name: /tools/i })).toBeInTheDocument();
    });
  });

  describe('History Controls', () => {
    it('renders undo and redo buttons', () => {
      render(Toolbar);
      
      expect(screen.getByRole('button', { name: /undo/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /redo/i })).toBeInTheDocument();
    });

    it('enables undo button when undo is available', () => {
      (signData as any).canUndo.mockReturnValue(true);
      render(Toolbar);
      
      const undoButton = screen.getByRole('button', { name: /undo/i });
      expect(undoButton).toBeEnabled();
    });

    it('disables undo button when undo is not available', () => {
      (signData as any).canUndo.mockReturnValue(false);
      render(Toolbar);
      
      const undoButton = screen.getByRole('button', { name: /undo/i });
      expect(undoButton).toBeDisabled();
    });

    it('enables redo button when redo is available', () => {
      (signData as any).canRedo.mockReturnValue(true);
      render(Toolbar);
      
      const redoButton = screen.getByRole('button', { name: /redo/i });
      expect(redoButton).toBeEnabled();
    });

    it('disables redo button when redo is not available', () => {
      (signData as any).canRedo.mockReturnValue(false);
      render(Toolbar);
      
      const redoButton = screen.getByRole('button', { name: /redo/i });
      expect(redoButton).toBeDisabled();
    });

    it('calls undo when undo button is clicked', async () => {
      (signData as any).canUndo.mockReturnValue(true);
      render(Toolbar);
      
      const undoButton = screen.getByRole('button', { name: /undo/i });
      await fireEvent.click(undoButton);
      
      expect(signData.undo).toHaveBeenCalled();
    });

    it('calls redo when redo button is clicked', async () => {
      (signData as any).canRedo.mockReturnValue(true);
      render(Toolbar);
      
      const redoButton = screen.getByRole('button', { name: /redo/i });
      await fireEvent.click(redoButton);
      
      expect(signData.redo).toHaveBeenCalled();
    });
  });

  describe('Clipboard Controls', () => {
    it('renders copy, cut, and paste buttons', () => {
      render(Toolbar);
      
      expect(screen.getByRole('button', { name: /copy/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /cut/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /paste/i })).toBeInTheDocument();
    });

    it('enables copy button when symbol is selected', () => {
      mockUiState.selectedSymbol = 'sym1';
      render(Toolbar);
      
      const copyButton = screen.getByRole('button', { name: /copy/i });
      expect(copyButton).toBeEnabled();
    });

    it('disables copy button when no symbol is selected', () => {
      mockUiState.selectedSymbol = null as any;
      render(Toolbar);
      
      const copyButton = screen.getByRole('button', { name: /copy/i });
      expect(copyButton).toBeDisabled();
    });

    it('enables paste button when clipboard has content', () => {
      canPaste.mockReturnValue(true);
      render(Toolbar);
      
      const pasteButton = screen.getByRole('button', { name: /paste/i });
      expect(pasteButton).toBeEnabled();
    });

    it('disables paste button when clipboard is empty', () => {
      canPaste.mockReturnValue(false);
      render(Toolbar);
      
      const pasteButton = screen.getByRole('button', { name: /paste/i });
      expect(pasteButton).toBeDisabled();
    });

    it('calls copy function when copy button is clicked', async () => {
      mockUiState.selectedSymbol = 'sym1';
      render(Toolbar);
      
      const copyButton = screen.getByRole('button', { name: /copy/i });
      await fireEvent.click(copyButton);
      
      expect(copySymbol).toHaveBeenCalledWith(mockSignData, 'sym1');
    });

    it('calls paste function when paste button is clicked', async () => {
      canPaste.mockReturnValue(true);
      render(Toolbar);
      
      const pasteButton = screen.getByRole('button', { name: /paste/i });
      await fireEvent.click(pasteButton);
      
      expect(pasteSymbol).toHaveBeenCalled();
    });

    it('removes symbol after cut operation', async () => {
      mockUiState.selectedSymbol = 'sym1';
      render(Toolbar);
      
      const cutButton = screen.getByRole('button', { name: /cut/i });
      await fireEvent.click(cutButton);
      
      expect(copySymbol).toHaveBeenCalledWith(mockSignData, 'sym1');
      expect(signData.update).toHaveBeenCalled();
    });
  });

  describe('Alignment Controls', () => {
    it('renders alignment buttons', () => {
      render(Toolbar);
      
      expect(screen.getByRole('button', { name: /align left/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /align center/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /align right/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /align top/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /align middle/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /align bottom/i })).toBeInTheDocument();
    });

    it('renders distribution buttons', () => {
      render(Toolbar);
      
      expect(screen.getByRole('button', { name: /distribute horizontally/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /distribute vertically/i })).toBeInTheDocument();
    });

    it('enables alignment buttons when multiple symbols are selected', () => {
      mockSignData.symbols[1].selected = true; // Select second symbol too
      render(Toolbar);
      
      const alignLeftButton = screen.getByRole('button', { name: /align left/i });
      expect(alignLeftButton).toBeEnabled();
    });

    it('disables alignment buttons when less than two symbols are selected', () => {
      render(Toolbar);
      
      const alignLeftButton = screen.getByRole('button', { name: /align left/i });
      expect(alignLeftButton).toBeDisabled();
    });

    it('calls alignment function when align button is clicked', async () => {
      mockSignData.symbols[1].selected = true;
      render(Toolbar);
      
      const alignLeftButton = screen.getByRole('button', { name: /align left/i });
      await fireEvent.click(alignLeftButton);
      
      expect(alignSymbols).toHaveBeenCalledWith(
        mockSignData.symbols.filter(s => s.selected),
        'left'
      );
    });

    it('calls distribution function when distribute button is clicked', async () => {
      mockSignData.symbols[1].selected = true;
      render(Toolbar);
      
      const distributeButton = screen.getByRole('button', { name: /distribute horizontally/i });
      await fireEvent.click(distributeButton);
      
      expect(distributeSymbols).toHaveBeenCalledWith(
        mockSignData.symbols.filter(s => s.selected),
        'horizontal'
      );
    });
  });

  describe('Tool Selection', () => {
    it('renders tool selection buttons', () => {
      render(Toolbar);
      
      expect(screen.getByRole('button', { name: /select tool/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /hand tool/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /zoom tool/i })).toBeInTheDocument();
    });

    it('highlights active tool', () => {
      mockUiState.tool = 'select';
      render(Toolbar);
      
      const selectButton = screen.getByRole('button', { name: /select tool/i });
      expect(selectButton).toHaveAttribute('aria-pressed', 'true');
    });

    it('changes tool when tool button is clicked', async () => {
      render(Toolbar);
      
      const handToolButton = screen.getByRole('button', { name: /hand tool/i });
      await fireEvent.click(handToolButton);
      
      expect(ui.tool.set).toHaveBeenCalledWith('hand');
    });
  });

  describe('Keyboard Shortcuts', () => {
    it('supports Ctrl+Z for undo', async () => {
      (signData as any).canUndo.mockReturnValue(true);
      const { container } = render(Toolbar);
      
      await fireEvent.keyDown(container, { key: 'z', ctrlKey: true });
      
      expect(signData.undo).toHaveBeenCalled();
    });

    it('supports Ctrl+Y for redo', async () => {
      (signData as any).canRedo.mockReturnValue(true);
      const { container } = render(Toolbar);
      
      await fireEvent.keyDown(container, { key: 'y', ctrlKey: true });
      
      expect(signData.redo).toHaveBeenCalled();
    });

    it('supports Ctrl+C for copy', async () => {
      mockUiState.selectedSymbol = 'sym1';
      const { container } = render(Toolbar);
      
      await fireEvent.keyDown(container, { key: 'c', ctrlKey: true });
      
      expect(copySymbol).toHaveBeenCalled();
    });

    it('supports Ctrl+V for paste', async () => {
      canPaste.mockReturnValue(true);
      const { container } = render(Toolbar);
      
      await fireEvent.keyDown(container, { key: 'v', ctrlKey: true });
      
      expect(pasteSymbol).toHaveBeenCalled();
    });

    it('supports Ctrl+X for cut', async () => {
      mockUiState.selectedSymbol = 'sym1';
      const { container } = render(Toolbar);
      
      await fireEvent.keyDown(container, { key: 'x', ctrlKey: true });
      
      expect(copySymbol).toHaveBeenCalled();
      expect(signData.update).toHaveBeenCalled();
    });
  });

  describe('Button States', () => {
    it('shows loading state during operations', async () => {
      // Mock a slow operation
      alignSymbols.mockImplementation(() => new Promise(resolve => setTimeout(resolve, 100)));
      
      mockSignData.symbols[1].selected = true;
      render(Toolbar);
      
      const alignButton = screen.getByRole('button', { name: /align left/i });
      await fireEvent.click(alignButton);
      
      expect(alignButton).toHaveAttribute('aria-busy', 'true');
    });

    it('shows tooltips with keyboard shortcuts', () => {
      render(Toolbar);
      
      const undoButton = screen.getByRole('button', { name: /undo/i });
      expect(undoButton).toHaveAttribute('title', expect.stringContaining('Ctrl+Z'));
    });

    it('updates button states reactively', () => {
      render(Toolbar);
      
      // Initially disabled
      const pasteButton = screen.getByRole('button', { name: /paste/i });
      expect(pasteButton).toBeDisabled();
      
      // Update clipboard state
      canPaste.mockReturnValue(true);
      mockUiState.clipboard = { symbol: 'S10000' } as any;
      
      // Re-render with new state
      render(Toolbar);
      
      const newPasteButton = screen.getByRole('button', { name: /paste/i });
      expect(newPasteButton).toBeEnabled();
    });
  });

  describe('Responsive Design', () => {
    it('collapses to compact mode on mobile', () => {
      Object.defineProperty(window, 'innerWidth', {
        writable: true,
        configurable: true,
        value: 375
      });
      
      render(Toolbar);
      
      const toolbar = screen.getByRole('toolbar', { name: /main toolbar/i });
      expect(toolbar).toHaveClass('compact');
    });

    it('shows overflow menu on small screens', () => {
      Object.defineProperty(window, 'innerWidth', {
        writable: true,
        configurable: true,
        value: 320
      });
      
      render(Toolbar);
      
      expect(screen.getByRole('button', { name: /more options/i })).toBeInTheDocument();
    });

    it('groups related tools together on mobile', () => {
      Object.defineProperty(window, 'innerWidth', {
        writable: true,
        configurable: true,
        value: 375
      });
      
      render(Toolbar);
      
      const toolGroups = screen.getAllByRole('group');
      expect(toolGroups.length).toBeGreaterThan(0);
      
      toolGroups.forEach(group => {
        expect(group).toHaveClass('mobile-group');
      });
    });
  });

  describe('Accessibility', () => {
    it('has proper ARIA roles and labels', () => {
      render(Toolbar);
      
      const toolbar = screen.getByRole('toolbar', { name: /main toolbar/i });
      expect(toolbar).toHaveAttribute('aria-label');
      
      const groups = screen.getAllByRole('group');
      groups.forEach(group => {
        expect(group).toHaveAttribute('aria-labelledby');
      });
    });

    it('supports keyboard navigation', async () => {
      render(Toolbar);
      
      const firstButton = screen.getByRole('button', { name: /undo/i });
      firstButton.focus();
      
      await fireEvent.keyDown(firstButton, { key: 'ArrowRight' });
      
      const secondButton = screen.getByRole('button', { name: /redo/i });
      expect(secondButton).toHaveFocus();
    });

    it('announces button state changes', () => {
      render(Toolbar);
      
      const selectButton = screen.getByRole('button', { name: /select tool/i });
      expect(selectButton).toHaveAttribute('aria-pressed');
    });

    it('provides descriptive button names', () => {
      render(Toolbar);
      
      const buttons = screen.getAllByRole('button');
      buttons.forEach(button => {
        expect(button).toHaveAccessibleName();
      });
    });
  });

  describe('Error Handling', () => {
    it('handles clipboard operation failures gracefully', async () => {
      copySymbol.mockRejectedValue(new Error('Clipboard access denied'));
      mockUiState.selectedSymbol = 'sym1';
      
      render(Toolbar);
      
      const copyButton = screen.getByRole('button', { name: /copy/i });
      await fireEvent.click(copyButton);
      
      // Should not crash and should show error feedback
      expect(screen.getByRole('alert')).toBeInTheDocument();
    });

    it('handles alignment operation failures', async () => {
      alignSymbols.mockRejectedValue(new Error('Alignment failed'));
      mockSignData.symbols[1].selected = true;
      
      render(Toolbar);
      
      const alignButton = screen.getByRole('button', { name: /align left/i });
      await fireEvent.click(alignButton);
      
      expect(screen.getByRole('alert')).toBeInTheDocument();
    });

    it('handles missing store data', () => {
      signData.subscribe.mockImplementation((callback) => {
        callback(null);
        return () => {};
      });
      
      expect(() => render(Toolbar)).not.toThrow();
    });
  });
});
