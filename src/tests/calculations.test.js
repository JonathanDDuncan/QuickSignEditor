/**
 * @fileoverview Tests for calculation functions
 * 100% coverage for all pure calculation functions
 */

import { describe, it, expect } from 'vitest';
import { 
  calculateBoundingBox,
  calculateDistance,
  isPointInBounds,
  calculateCenter
} from '../lib/utils/calculations.js';

describe('calculateBoundingBox', () => {
  it('calculates correct bounding box for symbols', () => {
    const symbols = [
      { x: 100, y: 150, width: 30, height: 40 },
      { x: 200, y: 100, width: 25, height: 35 }
    ];
    
    const result = calculateBoundingBox(symbols);
    expect(result).toEqual({
      x: 100,
      y: 100,
      width: 125, // 225 - 100
      height: 90  // 190 - 100
    });
  });

  it('handles empty symbol array', () => {
    const result = calculateBoundingBox([]);
    expect(result).toEqual({ x: 0, y: 0, width: 0, height: 0 });
  });

  it('handles null input', () => {
    const result = calculateBoundingBox(null);
    expect(result).toEqual({ x: 0, y: 0, width: 0, height: 0 });
  });

  it('handles single symbol', () => {
    const symbols = [{ x: 50, y: 75, width: 20, height: 30 }];
    const result = calculateBoundingBox(symbols);
    expect(result).toEqual({ x: 50, y: 75, width: 20, height: 30 });
  });
});

describe('calculateDistance', () => {
  it('calculates correct distance between points', () => {
    const point1 = { x: 0, y: 0 };
    const point2 = { x: 3, y: 4 };
    const result = calculateDistance(point1, point2);
    expect(result).toBe(5); // 3-4-5 triangle
  });

  it('returns zero for same points', () => {
    const point = { x: 100, y: 200 };
    const result = calculateDistance(point, point);
    expect(result).toBe(0);
  });

  it('handles null inputs', () => {
    const result = calculateDistance(null, { x: 10, y: 10 });
    expect(result).toBe(0);
  });
});

describe('isPointInBounds', () => {
  it('returns true for point inside bounds', () => {
    const point = { x: 150, y: 175 };
    const rect = { x: 100, y: 150, width: 100, height: 50 };
    const result = isPointInBounds(point, rect);
    expect(result).toBe(true);
  });

  it('returns false for point outside bounds', () => {
    const point = { x: 50, y: 50 };
    const rect = { x: 100, y: 150, width: 100, height: 50 };
    const result = isPointInBounds(point, rect);
    expect(result).toBe(false);
  });

  it('returns true for point on boundary', () => {
    const point = { x: 100, y: 150 };
    const rect = { x: 100, y: 150, width: 100, height: 50 };
    const result = isPointInBounds(point, rect);
    expect(result).toBe(true);
  });

  it('handles null inputs', () => {
    const result = isPointInBounds(null, { x: 0, y: 0, width: 100, height: 100 });
    expect(result).toBe(false);
  });
});

describe('calculateCenter', () => {
  it('calculates correct center point', () => {
    const rect = { x: 100, y: 150, width: 200, height: 100 };
    const result = calculateCenter(rect);
    expect(result).toEqual({ x: 200, y: 200 });
  });

  it('handles rectangle at origin', () => {
    const rect = { x: 0, y: 0, width: 10, height: 20 };
    const result = calculateCenter(rect);
    expect(result).toEqual({ x: 5, y: 10 });
  });

  it('handles null input', () => {
    const result = calculateCenter(null);
    expect(result).toEqual({ x: 0, y: 0 });
  });
});
