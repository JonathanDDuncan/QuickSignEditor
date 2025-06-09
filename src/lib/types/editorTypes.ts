export type EditorMode = 'select' | 'rectangleSelect' | 'circleSelect' | 'handCursor';

export interface SelectionArea {
  startX: number;
  startY: number;
  currentX: number;
  currentY: number;
  isSelecting: boolean;
}

export interface EditorState {
  mode: EditorMode;
  selectionArea: SelectionArea | null;
}
