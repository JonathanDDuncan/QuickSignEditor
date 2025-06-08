/**
 * @fileoverview Pure validation functions for SignWriting data
 * All functions are under 20 lines and side-effect free
 */

/**
 * Validates FSW (Formal SignWriting) format string
 * @param {string} fsw - FSW format string to validate
 * @returns {{isValid: boolean, error?: string}} Validation result
 */
export function validateFsw(fsw) {
  if (!fsw || typeof fsw !== 'string') {
    return { isValid: false, error: 'FSW must be a non-empty string' };
  }
  
  // Basic FSW pattern validation - starts with sign sequence
  const fswPattern = /^[AMBLRS]\d{3}x\d{3}/;
  if (!fswPattern.test(fsw)) {
    return { isValid: false, error: 'Invalid FSW format' };
  }
  
  return { isValid: true };
}

/**
 * Validates sign data structure
 * @param {Object} signData - Sign data object to validate
 * @returns {{isValid: boolean, error?: string}} Validation result
 */
export function validateSignData(signData) {
  if (!signData || typeof signData !== 'object') {
    return { isValid: false, error: 'Sign data must be an object' };
  }
  
  if (!signData.syms || !Array.isArray(signData.syms)) {
    return { isValid: false, error: 'Sign must have symbols array' };
  }
  
  return { isValid: true };
}

/**
 * Validates symbol structure
 * @param {Object} symbol - Symbol object to validate  
 * @returns {{isValid: boolean, error?: string}} Validation result
 */
export function validateSymbol(symbol) {
  if (!symbol || typeof symbol !== 'object') {
    return { isValid: false, error: 'Symbol must be an object' };
  }
  
  const requiredFields = ['key', 'x', 'y', 'width', 'height'];
  for (const field of requiredFields) {
    if (!(field in symbol)) {
      return { isValid: false, error: `Symbol missing required field: ${field}` };
    }
  }
  
  return { isValid: true };
}

/**
 * Validates position coordinates
 * @param {Object} position - Position object {x, y}
 * @returns {{isValid: boolean, error?: string}} Validation result
 */
export function validatePosition(position) {
  if (!position || typeof position !== 'object') {
    return { isValid: false, error: 'Position must be an object' };
  }
  
  if (typeof position.x !== 'number' || typeof position.y !== 'number') {
    return { isValid: false, error: 'Position x and y must be numbers' };
  }
  
  return { isValid: true };
}
