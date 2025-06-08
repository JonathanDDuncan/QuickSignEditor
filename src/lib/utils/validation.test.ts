/* eslint-disable no-undef */
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { validateSignData } from './validation';

describe('validateSignData', () => {
  beforeEach(() => {
    // Reset any global state if needed
  });

  afterEach(() => {
    // Clean up after each test if needed
  });

  it('returns valid for sign with name and symbols', () => {
    const sign = { name: 'Test', symbols: [{ id: 'a', x: 0, y: 0 }] };
    expect(validateSignData(sign)).toEqual({ isValid: true });
  });

  it('returns valid for sign with symbols but no name', () => {
    const sign = { name: '', symbols: [{ id: 'a', x: 0, y: 0 }] };
    expect(validateSignData(sign)).toEqual({ isValid: true });
  });

  it('returns invalid for sign with no name and no symbols', () => {
    const sign = { name: '', symbols: [] };
    expect(validateSignData(sign)).toEqual({ isValid: false, error: 'Name required' });
  });

  it('returns invalid for sign with only whitespace name and no symbols', () => {
    const sign = { name: '   ', symbols: [] };
    expect(validateSignData(sign)).toEqual({ isValid: false, error: 'Name required' });
  });

  it('returns invalid for missing name and missing symbols', () => {
    const input = {};
    const result = validateSignData(input);
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('Name required');
  });

  it('returns invalid for missing symbols', () => {
    const input = { name: 'test' };
    const result = validateSignData(input);
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('Symbols required');
  });

  it('returns invalid for non-array symbols', () => {
    const input = { name: 'test', symbols: null };
    const result = validateSignData(input);
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('Invalid symbols');
  });

  it('returns valid for empty symbols array (relaxed logic)', () => {
    const input = { name: 'test', symbols: [] };
    const result = validateSignData(input);
    expect(result.isValid).toBe(true);
  });

  it('returns valid for empty name but symbols present (relaxed logic)', () => {
    const input = { name: '', symbols: [{ id: 'a', x: 0, y: 0 }] };
    const result = validateSignData(input);
    expect(result.isValid).toBe(true);
  });

  it('returns invalid for null or undefined input', () => {
    expect(validateSignData(null).isValid).toBe(false);
    expect(validateSignData(undefined).isValid).toBe(false);
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
