import { render, screen, fireEvent, cleanup } from '@testing-library/svelte';
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import SignEditor from './SignEditor.svelte';

describe('SignEditor.svelte', () => {
  beforeEach(() => {
    cleanup();
  });

  it('shows error for invalid sign data', async () => {
    render(SignEditor, { signData: { name: '', symbols: [] } });
    expect(screen.getByText('Name required')).toBeInTheDocument();
  });

  it('renders canvas placeholder', async () => {
    render(SignEditor, { signData: { name: 'Test', symbols: [] } });
    expect(screen.getByLabelText('Sign canvas')).toBeInTheDocument();
    expect(screen.getByText('SignEditor canvas coming soon...')).toBeInTheDocument();
  });

  // Add a dummy test to ensure Vitest recognizes the suite
  it('dummy test', () => {
    expect(true).toBe(true);
  });
});
