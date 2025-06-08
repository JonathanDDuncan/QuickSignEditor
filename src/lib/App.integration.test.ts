import { render, screen, fireEvent, waitFor } from '@testing-library/svelte';
import { describe, it, expect, vi } from 'vitest';
import App from '../App.svelte';
import { signStore } from './signStore';

describe('Integrated UI', () => {
  it('renders the SignEditor and SymbolChooser and allows adding a symbol to the canvas', async () => {
    render(App);
    // Check initial symbols on canvas
    expect(screen.getByTestId('canvas-symbol-S1001')).toBeInTheDocument();
    expect(screen.getByTestId('canvas-symbol-S1002')).toBeInTheDocument();
    // Add a new symbol from the chooser
    const chooserBtn = screen.getByTestId('symbol-S1003');
    await fireEvent.click(chooserBtn);
    // The new symbol should now appear on the canvas
    expect(await screen.findByTestId('canvas-symbol-S1003')).toBeInTheDocument();
  });

  it('handles toolbar action events', async () => {
    // Reset store to initial state for a clean undo history
    signStore.set({
      name: 'Demo Sign',
      symbols: [
        { id: 'S1001', x: 50, y: 60, selected: false },
        { id: 'S1002', x: 120, y: 100, selected: false }
      ],
      metadata: {}
    });
    render(App);
    // Add a symbol to create undo history
    const chooserBtn = screen.getByTestId('symbol-S1003');
    await fireEvent.click(chooserBtn);
    // Confirm symbol added
    expect(await screen.findByTestId('canvas-symbol-S1003')).toBeInTheDocument();
    // There should be 3 symbols now
    expect(screen.getAllByTestId(/canvas-symbol-/).length).toBe(3);
    // Simulate undo
    const undoBtn = screen.getByTestId('undo-btn');
    await fireEvent.click(undoBtn);
    // Wait for the DOM to update after undo
    await waitFor(() => {
      expect(screen.queryByTestId('canvas-symbol-S1003')).not.toBeInTheDocument();
    }, { timeout: 2000 });
    // There should be 2 symbols now
    expect(screen.getAllByTestId(/canvas-symbol-/).length).toBe(2);
  });

  it('handles symbol selection from canvas', async () => {
    render(App);
    // Click a symbol on the canvas
    const symbolBtn = screen.getByTestId('canvas-symbol-S1001');
    await fireEvent.click(symbolBtn);
    // S1001 should be selected, S1002 not
    expect(symbolBtn).toHaveAttribute('aria-selected', 'true');
    const symbolBtn2 = screen.getByTestId('canvas-symbol-S1002');
    expect(symbolBtn2).toHaveAttribute('aria-selected', 'false');
  });
});
