import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { validateSignData } from './validation';

describe('validateSignData', () => {
  beforeEach(() => {
    // Reset any global state if needed
  });

  afterEach(() => {
    // Clean up after each test if needed
  });

  it('returns valid for complete data', () => {
    const input = { name: 'test', symbols: ['symbol1'] };
    const result = validateSignData(input);
    expect(result.isValid).toBe(true);
  });

  it('returns invalid for missing name', () => {
    const input = { symbols: ['symbol1'] };
    const result = validateSignData(input);
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('Name required');
  });

  it.each([
    [null, false, 'Invalid input'],
    [undefined, false, 'Invalid input'],
    [{}, false, 'Name required'],
    [{ name: '' }, false, 'Name required'],
    [{ name: 'test' }, false, 'Symbols required'],
    [{ name: 'test', symbols: null }, false, 'Invalid symbols'],
    [{ name: 'test', symbols: [] }, true, ''],
  ])('validates input %p correctly', (input, expectedValid, expectedError) => {
    const result = validateSignData(input);
    expect(result.isValid).toBe(expectedValid);
    if (!expectedValid) {
      expect(result.error).toContain(expectedError);
    }
  });

  it('performs well with large symbol arrays', () => {
    const largeSymbolArray = Array(1000).fill('').map((_, i) => `symbol${i}`);
    const input = { name: 'test', symbols: largeSymbolArray };
    const start = performance.now();
    const result = validateSignData(input);
    const end = performance.now();
    expect(result.isValid).toBe(true);
    expect(end - start).toBeLessThan(10);
  });
});
