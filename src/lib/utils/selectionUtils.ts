// Selection utility functions for rectangle and circle selection
import type { SelectionArea } from '../types/editorTypes';

export interface Rectangle {
  x: number;
  y: number;
  width: number;
  height: number;
}

export interface Circle {
  centerX: number;
  centerY: number;
  radius: number;
}

export interface Symbol {
  x: number;
  y: number;
  width?: number;
  height?: number;
  id: string;
}

/**
 * Check if a rectangle intersects with a selection rectangle
 */
export function isRectangleIntersect(symbolRect: Rectangle, selectionRect: Rectangle): boolean {
  return !(
    symbolRect.x > selectionRect.x + selectionRect.width ||
    symbolRect.x + symbolRect.width < selectionRect.x ||
    symbolRect.y > selectionRect.y + selectionRect.height ||
    symbolRect.y + symbolRect.height < selectionRect.y
  );
}

/**
 * Check if a rectangle is within a circle selection
 */
export function isRectangleInCircle(symbolRect: Rectangle, circle: Circle): boolean {
  // Check if the center of the rectangle is within the circle
  const rectCenterX = symbolRect.x + symbolRect.width / 2;
  const rectCenterY = symbolRect.y + symbolRect.height / 2;
  
  const distance = Math.sqrt(
    Math.pow(rectCenterX - circle.centerX, 2) + 
    Math.pow(rectCenterY - circle.centerY, 2)
  );
  
  return distance <= circle.radius;
}

/**
 * Get selection bounds from selection area (for rectangle selection)
 */
export function getSelectionBounds(selectionArea: SelectionArea): Rectangle {
  return {
    x: Math.min(selectionArea.startX, selectionArea.currentX),
    y: Math.min(selectionArea.startY, selectionArea.currentY),
    width: Math.abs(selectionArea.currentX - selectionArea.startX),
    height: Math.abs(selectionArea.currentY - selectionArea.startY)
  };
}

/**
 * Get circle bounds from selection area (for circle selection)
 */
export function getCircleBounds(selectionArea: SelectionArea): Circle {
  const centerX = selectionArea.startX;
  const centerY = selectionArea.startY;
  const radius = Math.sqrt(
    Math.pow(selectionArea.currentX - selectionArea.startX, 2) + 
    Math.pow(selectionArea.currentY - selectionArea.startY, 2)
  );
  
  return { centerX, centerY, radius };
}

/**
 * Get symbols within a rectangle selection
 */
export function getSymbolsInRectangle(symbols: Symbol[], selectionArea: SelectionArea): Symbol[] {
  return symbols.filter(symbol => {
    const symbolRect: Rectangle = {
      x: symbol.x,
      y: symbol.y,
      width: symbol.width || 44,
      height: symbol.height || 44
    };
    return isRectangleIntersectWithArea(symbolRect, selectionArea);
  });
}

/**
 * Get symbols within a circle selection
 */
export function getSymbolsInCircle(symbols: Symbol[], circle: Circle): Symbol[] {
  return symbols.filter(symbol => {
    const symbolRect: Rectangle = {
      x: symbol.x,
      y: symbol.y,
      width: symbol.width || 44,
      height: symbol.height || 44
    };
    return isRectangleInCircle(symbolRect, circle);
  });
}

/**
 * Check if a rectangle intersects with a selection area (convenience function)
 */
export function isRectangleIntersectWithArea(symbolRect: Rectangle, selectionArea: SelectionArea): boolean {
  const selectionRect = getSelectionBounds(selectionArea);
  return isRectangleIntersect(symbolRect, selectionRect);
}

/**
 * Check if a rectangle is within a circle selection area (convenience function)
 */
export function isRectangleInCircleWithArea(symbolRect: Rectangle, selectionArea: SelectionArea): boolean {
  const circle = getCircleBounds(selectionArea);
  return isRectangleInCircle(symbolRect, circle);
}
