import { describe, it, expect } from 'vitest';
import { isValidFSW } from './fsw';

describe('isValidFSW', () => {
  it('returns true for valid FSW sign strings', () => {
    expect(isValidFSW('M123abc')).toBe(true);
    expect(isValidFSW('S12345')).toBe(true);
    expect(isValidFSW('M123x456')).toBe(true);
    expect(isValidFSW('S1a2b3c')).toBe(true);
  });

  it('returns false for invalid FSW sign strings', () => {
    expect(isValidFSW('')).toBe(false);
    expect(isValidFSW('  ')).toBe(false);
    expect(isValidFSW(null)).toBe(false);
    expect(isValidFSW(undefined)).toBe(false);
    expect(isValidFSW('A12345')).toBe(false);
    expect(isValidFSW('M12G45')).toBe(false); // G is not hex
    expect(isValidFSW('S12345!')).toBe(false);
    expect(isValidFSW('S')).toBe(false);
    expect(isValidFSW('M')).toBe(false);
  });

  it('trims whitespace before validation', () => {
    expect(isValidFSW('  M123abc  ')).toBe(true);
  });

  // Add a dummy test to ensure Vitest recognizes the suite
  it('dummy test', () => {
    expect(true).toBe(true);
  });
});
