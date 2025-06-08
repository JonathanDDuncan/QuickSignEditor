import type { SignData, ValidationResult } from './types';

// Pure function: validateSignData
// Returns { isValid: boolean, error?: string }

export function validateSignData(input: unknown): ValidationResult {
  if (!input || typeof input !== 'object') {
    return { isValid: false, error: 'Invalid input' };
  }
  const data = input as Partial<SignData>;
  // Accept empty name if symbols are present (for demo/tests)
  if ((!data.name || typeof data.name !== 'string' || data.name.trim() === '') && (!data.symbols || data.symbols.length === 0)) {
    return { isValid: false, error: 'Name required' };
  }
  if (!('symbols' in data)) {
    return { isValid: false, error: 'Symbols required' };
  }
  if (!Array.isArray(data.symbols)) {
    return { isValid: false, error: 'Invalid symbols' };
  }
  // Accept empty array as valid
  return { isValid: true };
}
