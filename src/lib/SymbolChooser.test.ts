import { render, screen, fireEvent } from '@testing-library/svelte';
import { describe, it, expect, beforeEach } from 'vitest';
import SymbolChooser from './SymbolChooser.svelte';

describe('SymbolChooser.svelte', () => {
  const symbols = [
    { id: 'S10000', code: 'S10000', x: 0, y: 0 },
    { id: 'S10001', code: 'S10001', x: 1, y: 1 },
    { id: 'S20000', code: 'S20000', x: 2, y: 2 }
  ];

  it('renders all symbols by default', () => {
    render(SymbolChooser, { symbols });
    expect(screen.getByTestId('symbol-list').children.length).toBe(3);
  });

  it('filters symbols by search', async () => {
    render(SymbolChooser, { symbols });
    const input = screen.getByPlaceholderText('Search symbols...');
    await fireEvent.input(input, { target: { value: 'S1000' } });
    expect(screen.getByTestId('symbol-list').children.length).toBe(2);
  });

  it('shows no results if filter does not match', async () => {
    render(SymbolChooser, { symbols });
    const input = screen.getByPlaceholderText('Search symbols...');
    await fireEvent.input(input, { target: { value: 'ZZZ' } });
    expect(screen.getByText('No symbols found.')).toBeInTheDocument();
  });

  it('calls onSelect when a symbol is clicked', async () => {
    const onSelect = vi.fn();
    render(SymbolChooser, { symbols, onSelect });
    const btn = screen.getByTestId('symbol-S10000');
    await fireEvent.click(btn);
    expect(onSelect).toHaveBeenCalledWith(symbols[0]);
  });

  // Add a dummy test to ensure Vitest recognizes the suite
  it('dummy test', () => {
    expect(true).toBe(true);
  });
});
