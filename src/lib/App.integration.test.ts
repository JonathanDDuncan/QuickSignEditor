import { render, screen, fireEvent } from '@testing-library/svelte';
import { describe, it, expect } from 'vitest';
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
});
