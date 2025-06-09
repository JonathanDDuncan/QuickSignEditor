<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import type { SelectionArea } from '../types/editorTypes';

  export let selectionArea: SelectionArea | null = null;
  export let containerElement: HTMLElement | null = null;

  const dispatch = createEventDispatcher();

  $: rectangleStyle = selectionArea ? {
    left: Math.min(selectionArea.startX, selectionArea.currentX) + 'px',
    top: Math.min(selectionArea.startY, selectionArea.currentY) + 'px',
    width: Math.abs(selectionArea.currentX - selectionArea.startX) + 'px',
    height: Math.abs(selectionArea.currentY - selectionArea.startY) + 'px'
  } : {};

  function handleMouseDown(event: MouseEvent) {
    if (!containerElement) return;
    
    const rect = containerElement.getBoundingClientRect();
    const startX = event.clientX - rect.left;
    const startY = event.clientY - rect.top;
    
    const newSelectionArea: SelectionArea = {
      startX,
      startY,
      currentX: startX,
      currentY: startY,
      isSelecting: true
    };
    
    dispatch('selectionStart', newSelectionArea);
    
    function handleMouseMove(e: MouseEvent) {
      if (!newSelectionArea.isSelecting) return;
      
      const currentX = e.clientX - rect.left;
      const currentY = e.clientY - rect.top;
      
      dispatch('selectionMove', {
        ...newSelectionArea,
        currentX,
        currentY
      });
    }
    
    function handleMouseUp() {
      dispatch('selectionEnd');
      window.removeEventListener('mousemove', handleMouseMove);
      window.removeEventListener('mouseup', handleMouseUp);
    }
    
    window.addEventListener('mousemove', handleMouseMove);
    window.addEventListener('mouseup', handleMouseUp);
  }

  // Helper function to check if a rectangle intersects with another rectangle
  export function isRectangleIntersect(
    rect1: { x: number; y: number; width: number; height: number },
    rect2: { x: number; y: number; width: number; height: number }
  ): boolean {
    return !(
      rect1.x + rect1.width < rect2.x ||
      rect2.x + rect2.width < rect1.x ||
      rect1.y + rect1.height < rect2.y ||
      rect2.y + rect2.height < rect1.y
    );
  }

  // Calculate selection rectangle bounds
  export function getSelectionBounds(area: SelectionArea) {
    return {
      x: Math.min(area.startX, area.currentX),
      y: Math.min(area.startY, area.currentY),
      width: Math.abs(area.currentX - area.startX),
      height: Math.abs(area.currentY - area.startY)
    };
  }
</script>

<!-- Rectangle selection overlay -->
{#if selectionArea && selectionArea.isSelecting}
  <div 
    class="rectangle-selection"
    style="left: {rectangleStyle.left}; top: {rectangleStyle.top}; width: {rectangleStyle.width}; height: {rectangleStyle.height};"
    data-testid="rectangle-selection"
  />
{/if}

<!-- Invisible overlay to capture mouse events -->
<div 
  class="selection-overlay"
  on:mousedown={handleMouseDown}
  role="button"
  tabindex="0"
  aria-label="Rectangle selection area"
/>

<style>
.rectangle-selection {
  position: absolute;
  border: 2px dashed #0078d4;
  background: rgba(0, 120, 212, 0.1);
  pointer-events: none;
  z-index: 10;
  box-sizing: border-box;
}

.selection-overlay {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  cursor: crosshair;
  z-index: 5;
  outline: none;
}

.selection-overlay:focus {
  outline: 2px solid #0078d4;
  outline-offset: -2px;
}
</style>
