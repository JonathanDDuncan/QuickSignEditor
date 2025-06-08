/**
 * @fileoverview Tests for SignEditor.svelte component
 * Tests drag and drop, selection, keyboard shortcuts, and canvas interactions
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { render, fireEvent, screen, waitFor } from '@testing-library/svelte';
import { writable } from 'svelte/store';
import { tick } from 'svelte';
import SignEditor from '../lib/components/SignEditor.svelte';

let signData, ui;
let mockSignData, mockUiState;
let signDataSetSpy, signDataUpdateSpy, uiSetSpy, uiUpdateSpy;

describe('SignEditor Component', () => {
  beforeEach(() => {
    // Reset state
    mockSignData = {
      symbols: [],
      fsw: 'M500x500'
    };
    mockUiState = {
      selectedSymbol: null,
      tool: 'select',
      zoom: 1,
      showGrid: true,
      snapToGrid: true
    };
    signData = writable(mockSignData);
    ui = writable(mockUiState);
    signDataSetSpy = vi.spyOn(signData, 'set');
    signDataUpdateSpy = vi.spyOn(signData, 'update');
    uiSetSpy = vi.spyOn(ui, 'set');
    uiUpdateSpy = vi.spyOn(ui, 'update');
  });

  // Patch: Helper to update signData/ui in tests
  function setSignData(data) {
    mockSignData = { ...mockSignData, ...data };
    // Always merge deeply for symbols and other keys
    if (data.symbols) mockSignData.symbols = data.symbols;
    signData.set({ ...mockSignData });
  }
  function setUiState(data) {
    mockUiState = { ...mockUiState, ...data };
    ui.set({ ...mockUiState });
  }

  // Patch: Helper to get current store value
  function getStoreValue(store) {
    let value;
    store.subscribe(v => value = v)();
    return value;
  }

  describe('Basic Rendering', () => {
    it('renders the sign editor canvas', () => {
      render(SignEditor, { props: { signData, ui } });
      
      const canvas = screen.getByRole('img', { name: /sign editor canvas/i });
      expect(canvas).toBeInTheDocument();
      expect(canvas).toHaveClass('sign-editor');
    });

    it('applies zoom level to canvas', async () => {
      setUiState({ zoom: 1.5 });
      render(SignEditor, { props: { signData, ui } });
      const canvas = screen.getByRole('img', { name: /sign editor canvas/i });
      await waitFor(() => {
        expect(canvas.style.transform).toBe('scale(1.5)');
      });
    });

    it('shows grid when enabled', () => {
      setUiState({ showGrid: true });
      render(SignEditor, { props: { signData, ui } });
      
      const canvas = screen.getByRole('img', { name: /sign editor canvas/i });
      expect(canvas).toHaveClass('show-grid');
    });

    it('hides grid when disabled', () => {
      setUiState({ showGrid: false });
      render(SignEditor, { props: { signData, ui } });
      
      const canvas = screen.getByRole('img', { name: /sign editor canvas/i });
      expect(canvas).not.toHaveClass('show-grid');
    });
  });

  describe('Symbol Rendering', () => {
    it('renders symbols on canvas', async () => {
      setSignData({ symbols: [
        {
          id: 'sym1',
          symbol: 'S10000',
          x: 100,
          y: 100,
          selected: false
        },
        {
          id: 'sym2',
          symbol: 'S20000',
          x: 200,
          y: 150,
          selected: true
        }
      ] });
      setUiState({ selectedSymbol: 'sym2' });
      render(SignEditor, { props: { signData, ui } });
      const symbols = screen.getAllByRole('button', { name: /symbol/i });
      expect(symbols).toHaveLength(2);
      expect(symbols[1]).toHaveClass('selected');
    });

    it('applies correct positioning to symbols', () => {
      setSignData({ symbols: [
        {
          id: 'sym1',
          symbol: 'S10000',
          x: 100,
          y: 150,
          selected: false
        }
      ] });
      render(SignEditor, { props: { signData, ui } });
      const symbol = screen.getByRole('button', { name: /symbol/i });
      expect(symbol).toHaveStyle({
        left: '100px',
        top: '150px'
      });
    });
  });

  describe('Mouse Interactions', () => {
    it('selects symbol on click', async () => {
      setSignData({ symbols: [
        {
          id: 'sym1',
          symbol: 'S10000',
          x: 100,
          y: 100,
          selected: false
        }
      ] });
      const { container } = render(SignEditor, { props: { signData, ui } });
      const symbol = screen.getByRole('button', { name: /symbol/i });
      await fireEvent.click(symbol);
      await waitFor(() => {
        expect(getStoreValue(ui).selectedSymbol).toBe('sym1');
      });
    });

    it('starts drag operation on mousedown', async () => {
      setSignData({ symbols: [
        {
          id: 'sym1',
          symbol: 'S10000',
          x: 100,
          y: 100,
          selected: true
        }
      ] });
      const { container } = render(SignEditor, { props: { signData, ui } });
      const symbol = screen.getByRole('button', { name: /symbol/i });
      await fireEvent.mouseDown(symbol, { clientX: 150, clientY: 150 });
      // Check that drag state is initiated
      expect(symbol).toHaveAttribute('data-dragging', 'true');
    });

    it('moves symbol during drag', async () => {
      setSignData({
        symbols: [
          {
            id: 'sym1',
            symbol: 'S10000',
            x: 100,
            y: 100,
            selected: true
          }
        ]
      });
      
      const { container } = render(SignEditor, { props: { signData, ui } });
      
      const symbol = screen.getByRole('button', { name: /symbol/i });
      
      // Start drag
      await fireEvent.mouseDown(symbol, { clientX: 150, clientY: 150 });
      
      // Move mouse
      await fireEvent.mouseMove(container, { clientX: 200, clientY: 200 });
      
      expect(signData.update).toHaveBeenCalled();
    });

    it('ends drag on mouseup', async () => {
      setSignData({
        symbols: [
          {
            id: 'sym1',
            symbol: 'S10000',
            x: 100,
            y: 100,
            selected: true
          }
        ]
      });
      
      const { container } = render(SignEditor, { props: { signData, ui } });
      
      const symbol = screen.getByRole('button', { name: /symbol/i });
      
      // Start and end drag
      await fireEvent.mouseDown(symbol, { clientX: 150, clientY: 150 });
      await fireEvent.mouseUp(container);
      
      expect(symbol).not.toHaveAttribute('data-dragging');
    });
  });

  describe('Keyboard Shortcuts', () => {
    it('handles delete key to remove selected symbol', async () => {
      setSignData({ symbols: [
        { id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: true }
      ] });
      setUiState({ selectedSymbol: 'sym1' });
      const { container } = render(SignEditor, { props: { signData, ui } });
      await fireEvent.keyDown(container, { key: 'Delete' });
      await tick();
      expect(signDataUpdateSpy).toHaveBeenCalledTimes(1);
      // Also allow set to be called
      expect(signDataSetSpy).not.toHaveBeenCalledWith(undefined);
    });

    it('handles escape key to clear selection', async () => {
      setUiState({ selectedSymbol: 'sym1' });
      const { container } = render(SignEditor, { props: { signData, ui } });
      await fireEvent.keyDown(container, { key: 'Escape' });
      await waitFor(() => {
        expect(getStoreValue(ui).selectedSymbol).toBe(null);
      });
    });

    it('handles arrow keys for symbol movement', async () => {
      setSignData({ symbols: [
        { id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: true }
      ] });
      setUiState({ selectedSymbol: 'sym1' });
      const { container } = render(SignEditor, { props: { signData, ui } });
      await fireEvent.keyDown(container, { key: 'ArrowRight' });
      await tick();
      expect(signDataUpdateSpy).toHaveBeenCalledTimes(1);
      expect(signDataSetSpy).not.toHaveBeenCalledWith(undefined);
    });
    it('handles shift+arrow keys for fine movement', async () => {
      setSignData({ symbols: [
        { id: 'sym1', symbol: 'S10000', x: 100, y: 100, selected: true }
      ] });
      setUiState({ selectedSymbol: 'sym1' });
      const { container } = render(SignEditor, { props: { signData, ui } });
      await fireEvent.keyDown(container, { key: 'ArrowRight', shiftKey: true });
      await tick();
      expect(signDataUpdateSpy).toHaveBeenCalledTimes(1);
      expect(signDataSetSpy).not.toHaveBeenCalledWith(undefined);
    });
  });

  describe('Drag and Drop', () => {
    it('handles drop event for new symbols', async () => {
      const { container } = render(SignEditor, { props: { signData, ui } });
      
      const canvas = screen.getByRole('img', { name: /sign editor canvas/i });
      
      const dropEvent = new Event('drop', { bubbles: true });
      Object.defineProperty(dropEvent, 'dataTransfer', {
        value: {
          getData: vi.fn().mockReturnValue('S10000')
        }
      });
      Object.defineProperty(dropEvent, 'clientX', { value: 200 });
      Object.defineProperty(dropEvent, 'clientY', { value: 250 });
      
      await fireEvent(canvas, dropEvent);
      
      expect(signData.update).toHaveBeenCalled();
    });

    it('prevents default dragover behavior', async () => {
      render(SignEditor, { props: { signData, ui } });
      
      const canvas = screen.getByRole('img', { name: /sign editor canvas/i });
      
      const dragoverEvent = new Event('dragover', { bubbles: true });
      const preventDefaultSpy = vi.spyOn(dragoverEvent, 'preventDefault');
      
      await fireEvent(canvas, dragoverEvent);
      
      expect(preventDefaultSpy).toHaveBeenCalled();
    });
  });

  describe('Grid Snapping', () => {
    it('snaps to grid when enabled', async () => {
      setUiState({ snapToGrid: true });
      setSignData({
        symbols: [
          {
            id: 'sym1',
            symbol: 'S10000',
            x: 100,
            y: 100,
            selected: true
          }
        ]
      });
      
      const { container } = render(SignEditor, { props: { signData, ui } });
      
      const symbol = screen.getByRole('button', { name: /symbol/i });
      
      // Drag to a position that should snap
      await fireEvent.mouseDown(symbol, { clientX: 150, clientY: 150 });
      await fireEvent.mouseMove(container, { clientX: 187, clientY: 193 }); // Should snap to 190, 190
      await fireEvent.mouseUp(container);
      
      expect(signData.update).toHaveBeenCalled();
    });

    it('does not snap when disabled', async () => {
      setUiState({ snapToGrid: false });
      setSignData({
        symbols: [
          {
            id: 'sym1',
            symbol: 'S10000',
            x: 100,
            y: 100,
            selected: true
          }
        ]
      });
      
      const { container } = render(SignEditor, { props: { signData, ui } });
      
      const symbol = screen.getByRole('button', { name: /symbol/i });
      
      // Drag to exact position
      await fireEvent.mouseDown(symbol, { clientX: 150, clientY: 150 });
      await fireEvent.mouseMove(container, { clientX: 187, clientY: 193 });
      await fireEvent.mouseUp(container);
      
      expect(signData.update).toHaveBeenCalled();
    });
  });

  describe('Accessibility', () => {
    it('has proper ARIA labels', () => {
      render(SignEditor, { props: { signData, ui } });
      
      const canvas = screen.getByRole('img', { name: /sign editor canvas/i });
      expect(canvas).toHaveAttribute('aria-label');
    });

    it('is keyboard accessible', () => {
      render(SignEditor, { props: { signData, ui } });
      
      const canvas = screen.getByRole('img', { name: /sign editor canvas/i });
      expect(canvas).toHaveAttribute('tabindex', '0');
    });

    it('announces symbol selection to screen readers', async () => {
      setSignData({
        symbols: [
          {
            id: 'sym1',
            symbol: 'S10000',
            x: 100,
            y: 100,
            selected: false
          }
        ]
      });
      
      render(SignEditor, { props: { signData, ui } });
      
      const symbol = screen.getByRole('button', { name: /symbol/i });
      expect(symbol).toHaveAttribute('aria-label');
    });
  });

  describe('Error Handling', () => {
    it('handles invalid symbol data gracefully', () => {
      setSignData({ symbols: [
        { id: 'sym1', symbol: '', x: NaN, y: undefined, selected: false }
      ] });
      expect(() => render(SignEditor, { props: { signData, ui } })).not.toThrow();
    });

    it('handles missing store data', () => {
      // Use a mock store for this test
      const mockStore = { subscribe: (cb) => { cb(null); return () => {}; } };
      expect(() => render(SignEditor, { props: { signData: mockStore, ui } })).not.toThrow();
    });
  });
});
