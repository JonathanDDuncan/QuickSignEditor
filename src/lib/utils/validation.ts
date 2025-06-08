import type { SignData, ValidationResult } from './types';

// Pure function: validateSignData
// Returns { isValid: boolean, error?: string }

export function validateSignData(input: any): ValidationResult {
  if (!input || typeof input !== 'object') {
    return { isValid: false, error: 'Invalid input' };
  }
  if (!input.name || typeof input.name !== 'string' || input.name.trim() === '') {
    return { isValid: false, error: 'Name required' };
  }
  if (!('symbols' in input)) {
    return { isValid: false, error: 'Symbols required' };
  }
  if (!Array.isArray(input.symbols)) {
    return { isValid: false, error: 'Invalid symbols' };
  }
  // Accept empty array as valid
  return { isValid: true };
}
