/**
 * @fileoverview TypeScript type definitions for SignWriting
 * Comprehensive types for all SignWriting entities
 */

export interface Position {
  x: number;
  y: number;
}

export interface Size {
  width: number;
  height: number;
}

export interface Rectangle extends Position, Size {}

export interface Bounds {
  x: number;
  y: number;
  width: number;
  height: number;
}

export interface Symbol {
  key: string;
  x: number;
  y: number;
  width: number;
  height: number;
  id: number;
  selected: boolean;
  rotation?: number;
  fill?: number;
  color?: string;
}

export interface Sign {
  syms: symbol[];
  spelling?: string;
  width?: number;
  height?: number;
}

export interface ValidationResult {
  isValid: boolean;
  error?: string;
}

export interface FswData {
  symbols: string[];
  coordinates: Position[];
  raw: string;
}

export interface EditorState {
  sign: Sign;
  selectedSymbols: symbol[];
  viewPosition: Position;
  canvasSize: Size;
  editorMode: 'awaiting' | 'dragging' | 'selecting' | 'adding';
}

export interface UndoState {
  action: string;
  sign: Sign;
  timestamp: number;
}

export type Direction = 'up' | 'down' | 'left' | 'right';

export interface ViewPort extends Rectangle {
  name: string;
}
