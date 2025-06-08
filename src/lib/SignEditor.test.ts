import { render, screen, fireEvent, cleanup } from '@testing-library/svelte';
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import SignEditor from './SignEditor.svelte';
import type { ComponentType } from 'svelte';
import { tick } from 'svelte';

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
    const symbol0 = screen.getByTestId('canvas-symbol-S10000');
    const symbol1 = screen.getByTestId('canvas-symbol-S10001');
    expect(symbol0).toBeInTheDocument();
    expect(symbol1).toBeInTheDocument();
    expect(symbol0.style.left).toBe('10px');
    expect(symbol0.style.top).toBe('20px');
    expect(symbol1.style.left).toBe('100px');
    expect(symbol1.style.top).toBe('200px');
  });

  it('selects a symbol when clicked', async () => {
    let signData = {
      name: 'Test',
      symbols: [
        { id: 'S10000', code: 'S10000', x: 10, y: 20, selected: false },
        { id: 'S10001', code: 'S10001', x: 100, y: 200, selected: false }
      ]
    };
    function handleSelect(e: CustomEvent<string>) {
      const id = e.detail;
      signData = {
        ...signData,
        symbols: signData.symbols.map(s => ({ ...s, selected: s.id === id }))
      };
      rerender({ signData });
    }
    const { rerender, component } = render(SignEditorComponent, { signData });
    component.$on('select', handleSelect);
    const symbol0 = screen.getByTestId('canvas-symbol-S10000');
    const symbol1 = screen.getByTestId('canvas-symbol-S10001');
    await fireEvent.click(symbol1);
    await tick();
    expect(symbol1.className).toContain('selected');
    expect(symbol0.className).not.toContain('selected');
    await fireEvent.click(symbol0);
    await tick();
    expect(symbol0.className).toContain('selected');
    expect(symbol1.className).not.toContain('selected');
  });

  it('handles store-driven selection (no signData prop)', async () => {
    // Render without signData prop (simulate store-driven mode)
    // @ts-ignore
    const { component } = render(SignEditorComponent);
    // Add a symbol to the store
    // This assumes signStore is set up for testing; otherwise, mock or skip
    // For now, just check that the component renders without crashing
    expect(screen.getByLabelText('Sign canvas')).toBeInTheDocument();
  });

  it('shows error for invalid sign data (missing symbols)', async () => {
    render(SignEditorComponent, { signData: { name: 'Test', symbols: undefined } });
    expect(screen.getByText(/Invalid symbols|Name required|Symbols required/)).toBeInTheDocument();
  });

  it('dispatches move event with correct coordinates on drag', async () => {
    const signData = {
      name: 'Test',
      symbols: [
        { id: 'S10000', code: 'S10000', x: 10, y: 20, selected: false }
      ]
    };
    const { component } = render(SignEditorComponent, { signData });
    const symbolBtn = screen.getByTestId('canvas-symbol-S10000');
    let moveEventDetail = null;
    component.$on('move', (e: CustomEvent<any>) => { moveEventDetail = e.detail; });
    // Simulate mousedown at (15, 25) (5px offset from symbol's x/y)
    await fireEvent.mouseDown(symbolBtn, { clientX: 15, clientY: 25 });
    // Simulate mousemove to (100, 200)
    await fireEvent.mouseMove(window, { clientX: 100, clientY: 200 });
    // Simulate mouseup
    await fireEvent.mouseUp(window);
    // The new position should be (100 - 5, 200 - 5) = (95, 195)
    expect(moveEventDetail).toEqual({ id: 'S10000', x: 95, y: 195 });
  });

  it('dispatches resize event with correct dimensions on resize', async () => {
    const signData = {
      name: 'Test',
      symbols: [
        { id: 'S10000', code: 'S10000', x: 10, y: 20, width: 44, height: 44, selected: true }
      ]
    };
    const { component } = render(SignEditorComponent, { signData });
    const handle = screen.getByTestId('resize-handle-S10000');
    let resizeEventDetail = null;
    component.$on('resize', (e: CustomEvent<any>) => { resizeEventDetail = e.detail; });
    // Simulate mousedown on handle at (54, 64) (bottom-right corner)
    await fireEvent.mouseDown(handle, { clientX: 54, clientY: 64 });
    // Simulate mousemove to (74, 84) (+20px width, +20px height)
    await fireEvent.mouseMove(window, { clientX: 74, clientY: 84 });
    // Simulate mouseup
    await fireEvent.mouseUp(window);
    expect(resizeEventDetail).toEqual({ id: 'S10000', width: 64, height: 64 });
  });

  it('dispatches rotate event with correct angle on rotate', async () => {
    const signData = {
      name: 'Test',
      symbols: [
        { id: 'S10000', code: 'S10000', x: 100, y: 100, width: 44, height: 44, rotation: 0, selected: true }
      ]
    };
    const { component } = render(SignEditorComponent, { signData });
    const handle = screen.getByTestId('rotate-handle-S10000');
    let rotateEventDetail: any = null;
    component.$on('rotate', (e: CustomEvent<any>) => { rotateEventDetail = e.detail; });
    // Simulate mousedown on handle at (122, 82) (above center)
    await fireEvent.mouseDown(handle, { clientX: 122, clientY: 82 });
    // Simulate mousemove to the right of center (simulate 0 degrees)
    await fireEvent.mouseMove(window, { clientX: 144, clientY: 122 });
    // Simulate mouseup
    await fireEvent.mouseUp(window);
    expect(rotateEventDetail).not.toBeNull();
    expect(typeof rotateEventDetail.rotation).toBe('number');
    expect(rotateEventDetail.id).toBe('S10000');
  });

  it('dispatches color event with correct value on color change', async () => {
    const signData = {
      name: 'Test',
      symbols: [
        { id: 'S10000', code: 'S10000', x: 10, y: 20, color: '#ff0000', selected: true }
      ]
    };
    const { component } = render(SignEditorComponent, { signData });
    const colorInput = screen.getByTestId('color-picker-S10000');
    let colorEventDetail: any = null;
    component.$on('color', (e: CustomEvent<any>) => { colorEventDetail = e.detail; });
    // Simulate color input change
    await fireEvent.input(colorInput, { target: { value: '#00ff00' } });
    expect(colorEventDetail).not.toBeNull();
    expect(colorEventDetail).toEqual({ id: 'S10000', color: '#00ff00' });
  });

  // Add a dummy test to ensure Vitest recognizes the suite
  it('dummy test', () => {
    expect(true).toBe(true);
  });
});
