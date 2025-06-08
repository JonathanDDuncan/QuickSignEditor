/**
 * @fileoverview Pure calculation functions for SignWriting geometry
 * All functions are under 20 lines and side-effect free
 */

/**
 * Calculates bounding box for array of symbols
 * @param {Array} symbols - Array of symbol objects with x, y, width, height
 * @returns {Object} Bounding box {x, y, width, height}
 */
export function calculateBoundingBox(symbols) {
  if (!symbols || symbols.length === 0) {
    return { x: 0, y: 0, width: 0, height: 0 };
  }
  
  const minX = Math.min(...symbols.map(s => s.x));
  const minY = Math.min(...symbols.map(s => s.y));
  const maxX = Math.max(...symbols.map(s => s.x + s.width));
  const maxY = Math.max(...symbols.map(s => s.y + s.height));
  
  return {
    x: minX,
    y: minY,
    width: maxX - minX,
    height: maxY - minY
  };
}

/**
 * Calculates distance between two points
 * @param {Object} point1 - First point {x, y}
 * @param {Object} point2 - Second point {x, y}
 * @returns {number} Distance between points
 */
export function calculateDistance(point1, point2) {
  if (!point1 || !point2) return 0;
  
  const dx = point2.x - point1.x;
  const dy = point2.y - point1.y;
  return Math.sqrt(dx * dx + dy * dy);
}

/**
 * Checks if point is within rectangle bounds
 * @param {Object} point - Point {x, y}
 * @param {Object} rect - Rectangle {x, y, width, height}
 * @returns {boolean} True if point is within bounds
 */
export function isPointInBounds(point, rect) {
  if (!point || !rect) return false;
  
  return point.x >= rect.x && 
         point.x <= rect.x + rect.width &&
         point.y >= rect.y && 
         point.y <= rect.y + rect.height;
}

/**
 * Calculates center point of rectangle
 * @param {Object} rect - Rectangle {x, y, width, height}
 * @returns {Object} Center point {x, y}
 */
export function calculateCenter(rect) {
  if (!rect) return { x: 0, y: 0 };
  
  return {
    x: rect.x + rect.width / 2,
    y: rect.y + rect.height / 2
  };
}
