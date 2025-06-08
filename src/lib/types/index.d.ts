// Global and shared types for QuickSignEditor

export interface SignData {
  name: string;
  symbols: SymbolData[];
  metadata?: Record<string, any>;
}

export interface SymbolData {
  id: string;
  x: number;
  y: number;
  rotation?: number;
  color?: string;
  selected?: boolean;
}

export interface UIState {
  selectedSymbol: string | null;
  snapToGrid: boolean;
  currentTool: string;
  zoom: number;
}
