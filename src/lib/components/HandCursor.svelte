<script lang="ts">
  import { createEventDispatcher } from 'svelte';

  export let containerElement: HTMLElement | null = null;
  export let enabled = false;

  const dispatch = createEventDispatcher();

  let isPanning = false;
  let startX = 0;
  let startY = 0;
  let initialScrollLeft = 0;
  let initialScrollTop = 0;

  function handleMouseDown(event: MouseEvent) {
    if (!enabled || !containerElement) return;
    
    isPanning = true;
    startX = event.clientX;
    startY = event.clientY;
    initialScrollLeft = containerElement.scrollLeft;
    initialScrollTop = containerElement.scrollTop;
    
    dispatch('panStart', { x: startX, y: startY });
    
    function handleMouseMove(e: MouseEvent) {
      if (!isPanning || !containerElement) return;
      
      const deltaX = e.clientX - startX;
      const deltaY = e.clientY - startY;
      
      // Update scroll position
      containerElement.scrollLeft = initialScrollLeft - deltaX;
      containerElement.scrollTop = initialScrollTop - deltaY;
      
      dispatch('panMove', { 
        deltaX, 
        deltaY, 
        scrollLeft: containerElement.scrollLeft,
        scrollTop: containerElement.scrollTop
      });
    }
    
    function handleMouseUp() {
      isPanning = false;
      dispatch('panEnd');
      window.removeEventListener('mousemove', handleMouseMove);
      window.removeEventListener('mouseup', handleMouseUp);
    }
    
    window.addEventListener('mousemove', handleMouseMove);
    window.addEventListener('mouseup', handleMouseUp);
  }

  function handleKeyDown(event: KeyboardEvent) {
    if (!enabled || !containerElement) return;
    
    // Handle arrow keys for panning
    const panDistance = event.shiftKey ? 50 : 10;
    let deltaX = 0;
    let deltaY = 0;
    
    switch (event.key) {
      case 'ArrowLeft':
        deltaX = -panDistance;
        break;
      case 'ArrowRight':
        deltaX = panDistance;
        break;
      case 'ArrowUp':
        deltaY = -panDistance;
        break;
      case 'ArrowDown':
        deltaY = panDistance;
        break;
      default:
        return;
    }
    
    event.preventDefault();
    
    containerElement.scrollLeft += deltaX;
    containerElement.scrollTop += deltaY;
    
    dispatch('panMove', { 
      deltaX, 
      deltaY, 
      scrollLeft: containerElement.scrollLeft,
      scrollTop: containerElement.scrollTop
    });
  }

  // Helper function to reset pan position
  export function resetPan() {
    if (containerElement) {
      containerElement.scrollLeft = 0;
      containerElement.scrollTop = 0;
      dispatch('panReset');
    }
  }

  // Helper function to center the view
  export function centerView() {
    if (containerElement) {
      const centerX = (containerElement.scrollWidth - containerElement.clientWidth) / 2;
      const centerY = (containerElement.scrollHeight - containerElement.clientHeight) / 2;
      
      containerElement.scrollLeft = centerX;
      containerElement.scrollTop = centerY;
      
      dispatch('panCenter', { centerX, centerY });
    }
  }
</script>

<!-- Hand cursor overlay -->
{#if enabled}
  <div 
    class="hand-cursor-overlay"
    class:panning={isPanning}
    on:mousedown={handleMouseDown}
    on:keydown={handleKeyDown}
    role="button"
    tabindex="0"
    aria-label="Pan and navigate mode"
    data-testid="hand-cursor"
  />
{/if}

<style>
.hand-cursor-overlay {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  cursor: grab;
  z-index: 5;
  outline: none;
}

.hand-cursor-overlay:focus {
  outline: 2px solid #0078d4;
  outline-offset: -2px;
}

.hand-cursor-overlay.panning {
  cursor: grabbing;
}

.hand-cursor-overlay:active {
  cursor: grabbing;
}
</style>
