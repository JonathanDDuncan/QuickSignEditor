<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import type { SelectionArea } from '../types/editorTypes';

  export let selectionArea: SelectionArea | null = null;
  export let containerElement: HTMLElement | null = null;

  const dispatch = createEventDispatcher();

  $: circleStyle = selectionArea ? {
    centerX: (selectionArea.startX + selectionArea.currentX) / 2,
    centerY: (selectionArea.startY + selectionArea.currentY) / 2,
    radius: Math.sqrt(
      Math.pow(selectionArea.currentX - selectionArea.startX, 2) +
      Math.pow(selectionArea.currentY - selectionArea.startY, 2)
    ) / 2
  } : { centerX: 0, centerY: 0, radius: 0 };

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

  // Helper function to check if a point is within the circle
  export function isPointInCircle(
    point: { x: number; y: number },
    center: { x: number; y: number },
    radius: number
  ): boolean {
    const distance = Math.sqrt(
      Math.pow(point.x - center.x, 2) + Math.pow(point.y - center.y, 2)
    );
    return distance <= radius;
  }

  // Helper function to check if a rectangle intersects with a circle
  export function isRectangleInCircle(
    rect: { x: number; y: number; width: number; height: number },
    center: { x: number; y: number },
    radius: number
  ): boolean {
    // Check if any corner of the rectangle is within the circle
    const corners = [
      { x: rect.x, y: rect.y },
      { x: rect.x + rect.width, y: rect.y },
      { x: rect.x, y: rect.y + rect.height },
      { x: rect.x + rect.width, y: rect.y + rect.height }
    ];
    
    return corners.some(corner => isPointInCircle(corner, center, radius)) ||
           // Check if circle center is within the rectangle
           (center.x >= rect.x && center.x <= rect.x + rect.width &&
            center.y >= rect.y && center.y <= rect.y + rect.height);
  }

  // Calculate selection circle bounds
  export function getSelectionBounds(area: SelectionArea) {
    const centerX = (area.startX + area.currentX) / 2;
    const centerY = (area.startY + area.currentY) / 2;
    const radius = Math.sqrt(
      Math.pow(area.currentX - area.startX, 2) +
      Math.pow(area.currentY - area.startY, 2)
    ) / 2;
    
    return {
      centerX,
      centerY,
      radius,
      x: centerX - radius,
      y: centerY - radius,
      width: radius * 2,
      height: radius * 2
    };
  }
</script>

<!-- Circle selection overlay -->
{#if selectionArea && selectionArea.isSelecting}
  <div 
    class="circle-selection"
    style="left: {circleStyle.centerX - circleStyle.radius}px; top: {circleStyle.centerY - circleStyle.radius}px; width: {circleStyle.radius * 2}px; height: {circleStyle.radius * 2}px;"
    data-testid="circle-selection"
  />
{/if}

<!-- Invisible overlay to capture mouse events -->
<div 
  class="selection-overlay"
  on:mousedown={handleMouseDown}
  role="button"
  tabindex="0"
  aria-label="Circle selection area"
/>

<style>
.circle-selection {
  position: absolute;
  border: 2px dashed #0078d4;
  background: rgba(0, 120, 212, 0.1);
  border-radius: 50%;
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
