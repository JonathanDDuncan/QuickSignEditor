// TypeScript types for QuickSignEditor pure functions and state

// Symbol type for sign symbols
export interface Symbol {
  id: string;
  code?: string;
  x: number;
  y: number;
  rotation?: number;
  fill?: string;
  selected?: boolean;
  [key: string]: any;
}

// SignData type for a complete sign
export interface SignData {
  name: string;
  symbols: Symbol[];
  metadata?: Record<string, any>;
}

// UI state for editor
export interface UIState {
  selectedSymbol: string | null;
  selectedSymbols: string[];
  tool: 'select' | 'hand' | 'zoom' | 'pen';
  zoom: number;
  pan: { x: number; y: number };
  clipboard: Symbol[] | null;
  showGrid: boolean;
  snapToGrid: boolean;
}

// History state for undo/redo
export interface History<T> {
  past: T[];
  present: T;
  future: T[];
}

// Validation result
export interface ValidationResult {
  isValid: boolean;
  error?: string;
}
