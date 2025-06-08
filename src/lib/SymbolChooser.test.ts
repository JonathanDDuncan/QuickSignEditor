import { render, screen, fireEvent } from '@testing-library/svelte';
import { describe, it, expect, beforeEach } from 'vitest';
import { vi } from 'vitest';
import SymbolChooser from './SymbolChooser.svelte';
import type { ComponentType } from 'svelte';

const SymbolChooserComponent = SymbolChooser as unknown as ComponentType;

describe('SymbolChooser.svelte', () => {
  const symbols = [
    { id: 'S10000', code: 'S10000', x: 0, y: 0 },
    { id: 'S10001', code: 'S10001', x: 1, y: 1 },
    { id: 'S20000', code: 'S20000', x: 2, y: 2 }
  ];

  it('renders all symbols by default', () => {
    render(SymbolChooserComponent, { symbols });
    expect(screen.getByTestId('symbol-list').children.length).toBe(3);
  });

  it('filters symbols by search', async () => {
    render(SymbolChooserComponent, { symbols });
    const input = screen.getByPlaceholderText('Search symbols...');
    await fireEvent.input(input, { target: { value: 'S1000' } });
    expect(screen.getByTestId('symbol-list').children.length).toBe(2);
  });

  it('shows no results if filter does not match', async () => {
    render(SymbolChooserComponent, { symbols });
    const input = screen.getByPlaceholderText('Search symbols...');
    await fireEvent.input(input, { target: { value: 'ZZZ' } });
    expect(screen.getByText('No symbols found.')).toBeInTheDocument();
  });

  it('calls select event when a symbol is clicked', async () => {
    const { component } = render(SymbolChooserComponent, { symbols });
    const handler = vi.fn();
    component.$on('select', e => handler(e.detail));
    const btn = screen.getByTestId('symbol-S10000');
    await fireEvent.click(btn);
    expect(handler).toHaveBeenCalledWith(symbols[0]);
  });

  it('filters symbols when code is missing', async () => {
    const symbolsWithMissingCode = [
      { id: 'S1', x: 0, y: 0 },
      { id: 'S2', code: 'S2', x: 1, y: 1 }
    ];
    render(SymbolChooserComponent, { symbols: symbolsWithMissingCode });
    const input = screen.getByPlaceholderText('Search symbols...');
    await fireEvent.input(input, { target: { value: 'S1' } });
    expect(screen.getByTestId('symbol-list').children.length).toBe(1);
  });

  it('shows no results when symbols array is empty', () => {
    render(SymbolChooserComponent, { symbols: [] });
    expect(screen.getByText('No symbols found.')).toBeInTheDocument();
  });

  it('dummy test', () => {
    expect(true).toBe(true);
  });
});
