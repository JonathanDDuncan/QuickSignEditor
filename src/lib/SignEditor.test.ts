import { render, screen, fireEvent, cleanup } from '@testing-library/svelte';
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import SignEditor from './SignEditor.svelte';
import type { ComponentType } from 'svelte';

const SignEditorComponent = SignEditor as unknown as ComponentType;

describe('SignEditor.svelte', () => {
  beforeEach(() => {
    cleanup();
  });

  it('shows error for invalid sign data', async () => {
    render(SignEditorComponent, { signData: { name: '', symbols: [] } });
    expect(screen.getByText('Name required')).toBeInTheDocument();
  });

  it('renders canvas placeholder', async () => {
    render(SignEditorComponent, { signData: { name: 'Test', symbols: [] } });
    expect(screen.getByLabelText('Sign canvas')).toBeInTheDocument();
    expect(screen.getByText('SignEditor canvas coming soon...')).toBeInTheDocument();
  });

  it('renders symbols at correct positions', async () => {
    const signData = {
      name: 'Test',
      symbols: [
        { id: 'S10000', code: 'S10000', x: 10, y: 20 },
        { id: 'S10001', code: 'S10001', x: 100, y: 200 }
      ]
    };
    render(SignEditorComponent, { signData });
    const symbol0 = screen.getByTestId('symbol-S10000');
    const symbol1 = screen.getByTestId('symbol-S10001');
    expect(symbol0).toBeInTheDocument();
    expect(symbol1).toBeInTheDocument();
    expect(symbol0.style.left).toBe('10px');
    expect(symbol0.style.top).toBe('20px');
    expect(symbol1.style.left).toBe('100px');
    expect(symbol1.style.top).toBe('200px');
  });

  it('selects a symbol when clicked', async () => {
    const signData = {
      name: 'Test',
      symbols: [
        { id: 'S10000', code: 'S10000', x: 10, y: 20 },
        { id: 'S10001', code: 'S10001', x: 100, y: 200 }
      ]
    };
    render(SignEditorComponent, { signData });
    const symbol0 = screen.getByTestId('symbol-S10000');
    const symbol1 = screen.getByTestId('symbol-S10001');
    await fireEvent.click(symbol1);
    expect(symbol1.className).toContain('selected');
    expect(symbol0.className).not.toContain('selected');
    await fireEvent.click(symbol0);
    expect(symbol0.className).toContain('selected');
    expect(symbol1.className).not.toContain('selected');
  });

  // Add a dummy test to ensure Vitest recognizes the suite
  it('dummy test', () => {
    expect(true).toBe(true);
  });
});
