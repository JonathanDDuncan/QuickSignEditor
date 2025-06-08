import { render, screen } from '@testing-library/svelte';
import ChooserPanel from './ChooserPanel.svelte';
import { describe, it, expect } from 'vitest';

const baseModel = {
  groupselected: { symbolkey: 'S10000' },
  symbols: [
    { id: 'S10000', code: 'S10000', rowname: 'fist', category: 'Hand' },
    { id: 'S20000', code: 'S20000', rowname: 'circle', category: 'Hand' }
  ],
  filter: '',
  categories: ['Hand', 'Face'],
  selectedCategory: 'Hand',
  handGroupData: { label: 'Hand Group', items: [1, 2, 3] },
  generalGroupData: { label: 'General Group', items: [4, 5, 6] }
};

describe('ChooserPanel.svelte', () => {
  it('renders the maniquin preview area', () => {
    render(ChooserPanel, { model: baseModel });
    expect(screen.getByText(/Maniquin Preview/i)).toBeInTheDocument();
  });

  it('renders the main SymbolChooser with correct props', () => {
    render(ChooserPanel, { model: baseModel });
    expect(screen.getByPlaceholderText('Search symbols...')).toBeInTheDocument();
    expect(screen.getByTestId('symbol-list')).toBeInTheDocument();
    expect(screen.getByTestId('symbol-S10000')).toBeInTheDocument();
  });

  it('shows HandGroupChooser when groupselected is a hand', () => {
    render(ChooserPanel, { model: baseModel });
    // Use a more flexible matcher for the group label
    expect(screen.getByText('Hand Group')).toBeInTheDocument();
    expect(screen.queryByText('General Group')).not.toBeInTheDocument();
    // Check for group items
    expect(screen.getByText(/Hand Group Item: 1/)).toBeInTheDocument();
  });

  it('shows GeneralGroupChooser when groupselected is not a hand', () => {
    const model = { ...baseModel, groupselected: { symbolkey: 'S20000' } };
    render(ChooserPanel, { model });
    expect(screen.getByText('General Group')).toBeInTheDocument();
    expect(screen.queryByText('Hand Group')).not.toBeInTheDocument();
    // Check for group items
    expect(screen.getByText(/General Group Item: 4/)).toBeInTheDocument();
  });

  it('passes correct data to sub choosers', () => {
    render(ChooserPanel, { model: baseModel });
    expect(screen.getByText('Hand Group')).toBeInTheDocument();
    expect(screen.getByText(/Hand Group Item: 1/)).toBeInTheDocument();
    const model = { ...baseModel, groupselected: { symbolkey: 'S20000' } };
    render(ChooserPanel, { model });
    expect(screen.getByText('General Group')).toBeInTheDocument();
    expect(screen.getByText(/General Group Item: 4/)).toBeInTheDocument();
  });
});
