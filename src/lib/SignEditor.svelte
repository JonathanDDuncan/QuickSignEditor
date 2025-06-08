<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import { signStore } from './signStore';
  import { validateSignData } from './utils/validation';
  import transformSymbols from './utils/transform';
  import type { SignData, Symbol } from './utils/types';

  export let signData: SignData | undefined;
  let validation: { isValid: boolean; error?: string } = { isValid: true };
  const dispatch = createEventDispatcher();

  // Always derive symbols from signData or store
  $: currentSignData = signData ? signData : $signStore.present;
  $: symbols = transformSymbols(currentSignData.symbols);
  $: validation = validateSignData(currentSignData);
  $: showPlaceholder = symbols.length === 0 && validation.isValid;

  function selectSymbol(id: string, event?: MouseEvent | KeyboardEvent) {
    // Multi-select logic: Shift/Ctrl/Meta toggles selection
    const isMulti = event && (event.shiftKey || event.ctrlKey || event.metaKey);
    if (signData) {
      // Prop-driven: dispatch event for parent to handle selection
      dispatch('select', { id, multi: !!isMulti });
    } else {
      signStore.update(sd => {
        if (!isMulti) {
          // Single selection
          return {
            ...sd,
            symbols: sd.symbols.map(s => ({ ...s, selected: s.id === id }))
          };
        } else {
          // Toggle selection (multi): only toggle the clicked symbol, keep others as is
          return {
            ...sd,
            symbols: sd.symbols.map(s =>
              s.id === id ? { ...s, selected: !s.selected } : s
            )
          };
        }
      });
    }
    // Force Svelte reactivity for prop-driven mode
    if (signData && typeof signData === 'object' && 'symbols' in signData) {
      signData.symbols = signData.symbols.map(s =>
        (!isMulti && s.id !== id) ? { ...s, selected: false } : s
      );
    }
  }

  let draggingId: string | null = null;
  let dragOffset = { x: 0, y: 0 };
  let startPos = { x: 0, y: 0 };

  function onSymbolMouseDown(event: MouseEvent, symbol: Symbol) {
    draggingId = symbol.id;
    startPos = { x: symbol.x, y: symbol.y };
    dragOffset = {
      x: event.clientX - symbol.x,
      y: event.clientY - symbol.y
    };
    window.addEventListener('mousemove', onMouseMove);
    window.addEventListener('mouseup', onMouseUp);
  }

  function onMouseMove(event: MouseEvent) {
    if (!draggingId) return;
    const newX = event.clientX - dragOffset.x;
    const newY = event.clientY - dragOffset.y;
    if (signData) {
      // Prop-driven: dispatch event for parent to handle position update
      dispatch('move', { id: draggingId, x: newX, y: newY });
    } else {
      // Store-driven: update position in the store
      signStore.update(sd => ({
        ...sd,
        symbols: sd.symbols.map(s =>
          s.id === draggingId ? { ...s, x: newX, y: newY } : s
        )
      }));
    }
  }

  function onMouseUp() {
    draggingId = null;
    window.removeEventListener('mousemove', onMouseMove);
    window.removeEventListener('mouseup', onMouseUp);
  }

  // Add resizing state
  let resizingId: string | null = null;
  let resizeStart = { x: 0, y: 0, width: 0, height: 0 };

  function onResizeHandleMouseDown(event: MouseEvent, symbol: Symbol) {
    event.stopPropagation();
    resizingId = symbol.id;
    resizeStart = {
      x: event.clientX,
      y: event.clientY,
      width: symbol.width || 44,
      height: symbol.height || 44
    };
    window.addEventListener('mousemove', onResizeMouseMove);
    window.addEventListener('mouseup', onResizeMouseUp);
  }

  function onResizeMouseMove(event: MouseEvent) {
    if (!resizingId) return;
    const dx = event.clientX - resizeStart.x;
    const dy = event.clientY - resizeStart.y;
    const newWidth = Math.max(20, resizeStart.width + dx);
    const newHeight = Math.max(20, resizeStart.height + dy);
    if (signData) {
      dispatch('resize', { id: resizingId, width: newWidth, height: newHeight });
    } else {
      signStore.update(sd => ({
        ...sd,
        symbols: sd.symbols.map(s =>
          s.id === resizingId ? { ...s, width: newWidth, height: newHeight } : s
        )
      }));
    }
  }

  function onResizeMouseUp() {
    resizingId = null;
    window.removeEventListener('mousemove', onResizeMouseMove);
    window.removeEventListener('mouseup', onResizeMouseUp);
  }

  // Add rotation state
  let rotatingId: string | null = null;
  let rotateStart = { x: 0, y: 0, angle: 0, centerX: 0, centerY: 0 };

  function onRotateHandleMouseDown(event: MouseEvent, symbol: Symbol) {
    event.stopPropagation();
    rotatingId = symbol.id;
    const rect = (event.target as HTMLElement).parentElement?.getBoundingClientRect();
    if (rect) {
      rotateStart = {
        x: event.clientX,
        y: event.clientY,
        angle: symbol.rotation || 0,
        centerX: rect.left + (rect.width / 2),
        centerY: rect.top + (rect.height / 2)
      };
      window.addEventListener('mousemove', onRotateMouseMove);
      window.addEventListener('mouseup', onRotateMouseUp);
    }
  }

  function onRotateMouseMove(event: MouseEvent) {
    if (!rotatingId) return;
    const dx = event.clientX - rotateStart.centerX;
    const dy = event.clientY - rotateStart.centerY;
    const angleRad = Math.atan2(dy, dx);
    const angleDeg = (angleRad * 180) / Math.PI;
    const newAngle = Math.round(angleDeg);
    if (signData) {
      dispatch('rotate', { id: rotatingId, rotation: newAngle });
    } else {
      signStore.update(sd => ({
        ...sd,
        symbols: sd.symbols.map(s =>
          s.id === rotatingId ? { ...s, rotation: newAngle } : s
        )
      }));
    }
  }

  function onRotateMouseUp() {
    rotatingId = null;
    window.removeEventListener('mousemove', onRotateMouseMove);
    window.removeEventListener('mouseup', onRotateMouseUp);
  }

  function onColorChange(event: Event, symbol: Symbol) {
    const color = (event.target as HTMLInputElement).value;
    if (signData) {
      dispatch('color', { id: symbol.id, color });
    } else {
      signStore.update(sd => ({
        ...sd,
        symbols: sd.symbols.map(s =>
          s.id === symbol.id ? { ...s, color } : s
        )
      }));
    }
  }

  // Keyboard accessibility for symbol manipulation
  function onKeyDown(event: KeyboardEvent) {
    if (!symbols.length) return;
    const selectedIdx = symbols.findIndex(s => s.selected);
    if (selectedIdx === -1) return;
    const selected = symbols[selectedIdx];
    let handled = false;
    // Move with arrow keys
    let dx = 0, dy = 0;
    if (event.key === 'ArrowLeft') { dx = -5; handled = true; }
    if (event.key === 'ArrowRight') { dx = 5; handled = true; }
    if (event.key === 'ArrowUp') { dy = -5; handled = true; }
    if (event.key === 'ArrowDown') { dy = 5; handled = true; }
    if (handled) {
      const selectedSymbols = symbols.filter(s => s.selected);
      if (signData) {
        selectedSymbols.forEach(sel => {
          dispatch('move', { id: sel.id, x: sel.x + dx, y: sel.y + dy });
        });
      } else {
        signStore.update(sd => ({
          ...sd,
          symbols: sd.symbols.map(s =>
            s.selected ? { ...s, x: s.x + dx, y: s.y + dy } : s
          )
        }));
      }
      event.preventDefault();
      return;
    }
    // Delete selected symbol(s)
    if (event.key === 'Delete' || event.key === 'Backspace') {
      const selectedSymbols = symbols.filter(s => s.selected);
      if (signData) {
        selectedSymbols.forEach(sel => {
          dispatch('delete', { id: sel.id });
        });
      } else {
        signStore.update(sd => ({
          ...sd,
          symbols: sd.symbols.filter(s => !s.selected)
        }));
      }
      event.preventDefault();
      return;
    }
    // Tab/Shift+Tab to cycle selection
    if (event.key === 'Tab') {
      let nextIdx = event.shiftKey ? (selectedIdx - 1 + symbols.length) % symbols.length : (selectedIdx + 1) % symbols.length;
      const nextId = symbols[nextIdx].id;
      selectSymbol(nextId, event); // clear others
      event.preventDefault();
    }
  }
</script>

<div class="sign-editor-canvas" aria-label="Sign canvas" tabindex="0" on:keydown={onKeyDown}>
  {#if showPlaceholder}
    <div class="placeholder">SignEditor canvas coming soon...</div>
  {/if}
  {#each symbols as symbol (symbol.id)}
    <button
      type="button"
      class="symbol"
      class:selected={symbol.selected}
      data-testid={`canvas-symbol-${symbol.id}`}
      style="left: {symbol.x}px; top: {symbol.y}px; width: {symbol.width || 44}px; height: {symbol.height || 44}px; transform: rotate({symbol.rotation || 0}deg); background: {symbol.color || '#fff'};"
      aria-selected={symbol.selected ? 'true' : 'false'}
      role="option"
      on:click|stopPropagation={(e) => selectSymbol(symbol.id, e)}
      on:mousedown|stopPropagation={(e) => onSymbolMouseDown(e, symbol)}
      draggable="false"
    >
      <!-- Render symbol SVG or image here -->
      <span>{symbol.id}</span>
      {#if symbol.selected}
        <div
          class="resize-handle"
          data-testid={`resize-handle-${symbol.id}`}
          on:mousedown|stopPropagation={(e) => onResizeHandleMouseDown(e, symbol)}
        />
        <div
          class="rotate-handle"
          data-testid={`rotate-handle-${symbol.id}`}
          on:mousedown|stopPropagation={(e) => onRotateHandleMouseDown(e, symbol)}
        >
          &#8635;
        </div>
        <input
          type="color"
          class="color-picker"
          data-testid={`color-picker-${symbol.id}`}
          value={symbol.color || '#ffffff'}
          on:input={(e) => onColorChange(e, symbol)}
          aria-label="Pick symbol color"
        />
      {/if}
    </button>
  {/each}
  {#if !validation.isValid}
    <div class="error">{validation.error}</div>
  {/if}
</div>

<style>
.sign-editor-canvas {
  position: relative;
  width: 100%;
  height: 400px;
  background: #f8f8f8;
  border: 1px solid #ccc;
  overflow: auto;
  outline: none;
  min-width: 0;
}
.symbol {
  /* Add relative for handle positioning */
  position: absolute;
  cursor: pointer;
  user-select: none;
  padding: 8px 10px;
  background: #fff;
  border: 1px solid #bbb;
  border-radius: 3px;
  font-size: 16px;
  transition: box-shadow 0.1s;
  min-width: 44px;
  min-height: 44px;
  box-sizing: border-box;
  overflow: visible;
}
.symbol[aria-selected="true"], .symbol.selected {
  box-shadow: 0 0 0 2px #0078d4;
  background: #e6f0fa;
}
.symbol:focus {
  outline: 2px solid #0078d4;
  z-index: 1;
}
@media (max-width: 800px) {
  .sign-editor-canvas {
    height: 300px;
  }
  .symbol {
    font-size: 15px;
    padding: 8px 8px;
    min-width: 40px;
    min-height: 40px;
  }
}
@media (max-width: 500px) {
  .sign-editor-canvas {
    height: 220px;
  }
  .symbol {
    font-size: 14px;
    padding: 6px 6px;
    min-width: 36px;
    min-height: 36px;
  }
}
.error {
  position: absolute;
  bottom: 8px;
  left: 8px;
  color: #b00;
  background: #fff0f0;
  padding: 4px 8px;
  border-radius: 3px;
  font-size: 13px;
  border: 1px solid #fbb;
}
.placeholder {
  color: #888;
  text-align: center;
  margin-top: 160px;
  font-size: 1.2em;
}
.resize-handle {
  position: absolute;
  right: 0;
  bottom: 0;
  width: 16px;
  height: 16px;
  background: #0078d4;
  border-radius: 0 0 3px 0;
  cursor: se-resize;
  z-index: 2;
}
.rotate-handle {
  position: absolute;
  top: -18px;
  left: 50%;
  transform: translateX(-50%);
  width: 20px;
  height: 20px;
  background: #fff;
  border: 2px solid #0078d4;
  border-radius: 50%;
  color: #0078d4;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 16px;
  cursor: grab;
  z-index: 3;
  box-shadow: 0 1px 4px rgba(0,0,0,0.08);
  user-select: none;
}
.rotate-handle:active {
  cursor: grabbing;
}
.color-picker {
  position: absolute;
  left: 50%;
  bottom: -28px;
  transform: translateX(-50%);
  width: 28px;
  height: 28px;
  border: none;
  background: none;
  cursor: pointer;
  z-index: 4;
  box-shadow: 0 1px 4px rgba(0,0,0,0.08);
}
</style>
