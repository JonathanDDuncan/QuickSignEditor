/**
 * @fileoverview Tests for validation functions
 * 100% coverage for all pure validation functions
 */

import { describe, it, expect } from 'vitest';
import { 
  validateFsw,
  validateSignData,
  validateSymbol,
  validatePosition
} from '../lib/utils/validation.js';

describe('validateFsw', () => {
  it('returns valid for correct FSW format', () => {
    const result = validateFsw('M500x500S10000490x510');
    expect(result.isValid).toBe(true);
  });

  it('returns invalid for null input', () => {
    const result = validateFsw(null);
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('FSW must be a non-empty string');
  });

  it('returns invalid for empty string', () => {
    const result = validateFsw('');
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('FSW must be a non-empty string');
  });

  it('returns invalid for non-string input', () => {
    const result = validateFsw(123);
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('FSW must be a non-empty string');
  });

  it('returns invalid for incorrect FSW format', () => {
    const result = validateFsw('invalid-fsw');
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('Invalid FSW format');
  });
});

describe('validateSignData', () => {
  it('returns valid for correct sign data', () => {
    const signData = { syms: [] };
    const result = validateSignData(signData);
    expect(result.isValid).toBe(true);
  });

  it('returns invalid for null input', () => {
    const result = validateSignData(null);
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('Sign data must be an object');
  });

  it('returns invalid for non-object input', () => {
    const result = validateSignData('not-object');
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('Sign data must be an object');
  });

  it('returns invalid for missing syms array', () => {
    const result = validateSignData({});
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('Sign must have symbols array');
  });

  it('returns invalid for non-array syms', () => {
    const result = validateSignData({ syms: 'not-array' });
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('Sign must have symbols array');
  });
});

describe('validateSymbol', () => {
  it('returns valid for complete symbol', () => {
    const symbol = { key: 'S10000', x: 100, y: 200, width: 30, height: 30 };
    const result = validateSymbol(symbol);
    expect(result.isValid).toBe(true);
  });

  it('returns invalid for null input', () => {
    const result = validateSymbol(null);
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('Symbol must be an object');
  });

  it('returns invalid for missing required fields', () => {
    const symbol = { key: 'S10000' }; // Missing x, y, width, height
    const result = validateSymbol(symbol);
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('Symbol missing required field: x');
  });
});

describe('validatePosition', () => {
  it('returns valid for correct position', () => {
    const position = { x: 100, y: 200 };
    const result = validatePosition(position);
    expect(result.isValid).toBe(true);
  });

  it('returns invalid for null input', () => {
    const result = validatePosition(null);
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('Position must be an object');
  });

  it('returns invalid for non-numeric coordinates', () => {
    const position = { x: 'not-number', y: 200 };
    const result = validatePosition(position);
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('Position x and y must be numbers');
  });
});
