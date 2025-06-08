import { render, fireEvent, screen } from '@testing-library/svelte';
import { describe, it, expect, vi } from 'vitest';
import Toolbar from './Toolbar.svelte';

describe('Toolbar.svelte', () => {
  it('calls onUndo when Undo button is clicked', async () => {
    const onUndo = vi.fn();
    render(Toolbar, { onUndo });
    await fireEvent.click(screen.getByTestId('undo-btn'));
    expect(onUndo).toHaveBeenCalled();
  });

  it('calls onRedo when Redo button is clicked', async () => {
    const onRedo = vi.fn();
    render(Toolbar, { onRedo });
    await fireEvent.click(screen.getByTestId('redo-btn'));
    expect(onRedo).toHaveBeenCalled();
  });

  it('calls onCopy when Copy button is clicked', async () => {
    const onCopy = vi.fn();
    render(Toolbar, { onCopy });
    await fireEvent.click(screen.getByTestId('copy-btn'));
    expect(onCopy).toHaveBeenCalled();
  });

  it('calls onPaste when Paste button is clicked', async () => {
    const onPaste = vi.fn();
    render(Toolbar, { onPaste });
    await fireEvent.click(screen.getByTestId('paste-btn'));
    expect(onPaste).toHaveBeenCalled();
  });

  it('calls onAlign when Align button is clicked', async () => {
    const onAlign = vi.fn();
    render(Toolbar, { onAlign });
    await fireEvent.click(screen.getByTestId('align-btn'));
    expect(onAlign).toHaveBeenCalled();
  });

  it('calls onImportFSW when Import FSW button is clicked', async () => {
    const onImportFSW = vi.fn();
    render(Toolbar, { onImportFSW });
    await fireEvent.click(screen.getByTestId('import-fsw-btn'));
    expect(onImportFSW).toHaveBeenCalled();
  });

  it('calls onExportFSW when Export FSW button is clicked', async () => {
    const onExportFSW = vi.fn();
    render(Toolbar, { onExportFSW });
    await fireEvent.click(screen.getByTestId('export-fsw-btn'));
    expect(onExportFSW).toHaveBeenCalled();
  });

  // Add a dummy test to ensure Vitest recognizes the suite
  it('dummy test', () => {
    expect(true).toBe(true);
  });
});
