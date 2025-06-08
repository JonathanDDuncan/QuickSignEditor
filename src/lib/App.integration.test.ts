import { render, screen, fireEvent } from '@testing-library/svelte';
import { describe, it, expect, vi } from 'vitest';
import App from '../App.svelte';

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
    // Mock window.alert
    const alertSpy = vi.spyOn(window, 'alert').mockImplementation(() => {});
    render(App);
    const toolbar = screen.getByRole('toolbar');
    // Simulate a toolbar action (e.g., undo)
    const undoBtn = screen.getByTestId('undo-btn');
    await fireEvent.click(undoBtn);
    expect(alertSpy).toHaveBeenCalledWith(expect.stringContaining('undo'));
    alertSpy.mockRestore();
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
