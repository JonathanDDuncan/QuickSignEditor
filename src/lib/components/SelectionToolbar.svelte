<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import type { EditorMode } from '../types/editorTypes';

  export let currentMode: EditorMode = 'select';

  const dispatch = createEventDispatcher();

  interface ToolButton {
    mode: EditorMode;
    icon: string;
    label: string;
    title: string;
    shortcut?: string;
  }

  const tools: ToolButton[] = [
    {
      mode: 'select',
      icon: '↖',
      label: 'Select',
      title: 'Select tool - Click to select symbols',
      shortcut: 'V'
    },
    {
      mode: 'rectangleSelect',
      icon: '⬚',
      label: 'Rectangle',
      title: 'Rectangle select - Drag to select symbols in a rectangle',
      shortcut: 'R'
    },
    {
      mode: 'circleSelect',
      icon: '○',
      label: 'Circle',
      title: 'Circle select - Drag to select symbols in a circle',
      shortcut: 'C'
    },
    {
      mode: 'handCursor',
      icon: '✋',
      label: 'Pan',
      title: 'Hand tool - Pan and navigate the canvas',
      shortcut: 'H'
    }
  ];

  function selectTool(mode: EditorMode) {
    if (currentMode === mode) return;
    currentMode = mode;
    dispatch('modeChange', mode);
  }

  function handleKeyDown(event: KeyboardEvent) {
    // Handle tool shortcuts
    const tool = tools.find(t => t.shortcut?.toLowerCase() === event.key.toLowerCase());
    if (tool) {
      event.preventDefault();
      selectTool(tool.mode);
    }
  }

  // Handle escape to return to select mode
  function handleEscape(event: KeyboardEvent) {
    if (event.key === 'Escape' && currentMode !== 'select') {
      event.preventDefault();
      selectTool('select');
    }
  }
</script>

<svelte:window on:keydown={handleKeyDown} on:keydown={handleEscape} />

<div class="selection-toolbar" role="toolbar" aria-label="Selection tools">
  {#each tools as tool (tool.mode)}
    <button
      type="button"
      class="tool-button"
      class:active={currentMode === tool.mode}
      on:click={() => selectTool(tool.mode)}
      title="{tool.title}{tool.shortcut ? ` (${tool.shortcut})` : ''}"
      aria-label={tool.title}
      aria-pressed={currentMode === tool.mode}
      data-testid={`tool-${tool.mode}`}
    >
      <span class="tool-icon" aria-hidden="true">{tool.icon}</span>
      <span class="tool-label">{tool.label}</span>
      {#if tool.shortcut}
        <span class="tool-shortcut" aria-hidden="true">{tool.shortcut}</span>
      {/if}
    </button>
  {/each}
</div>

<style>
.selection-toolbar {
  display: flex;
  gap: 0.25rem;
  background: #f8f8f8;
  border: 1px solid #ccc;
  border-radius: 4px;
  padding: 0.25rem;
  align-items: center;
  flex-wrap: wrap;
}

.tool-button {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0.1rem;
  background: #fff;
  border: 1px solid #bbb;
  border-radius: 3px;
  padding: 0.4rem 0.6rem;
  font-size: 0.8rem;
  cursor: pointer;
  transition: all 0.2s;
  min-width: 50px;
  position: relative;
}

.tool-button:hover, .tool-button:focus {
  background: #e6f0fa;
  border-color: #0078d4;
  outline: none;
}

.tool-button.active {
  background: #0078d4;
  color: white;
  border-color: #005a9e;
}

.tool-button.active:hover {
  background: #106ebe;
}

.tool-icon {
  font-size: 1.2rem;
  line-height: 1;
}

.tool-label {
  font-size: 0.75rem;
  font-weight: 500;
  text-align: center;
  white-space: nowrap;
}

.tool-shortcut {
  position: absolute;
  top: 2px;
  right: 2px;
  font-size: 0.6rem;
  opacity: 0.7;
  font-weight: bold;
  background: rgba(0, 0, 0, 0.1);
  padding: 1px 3px;
  border-radius: 2px;
  line-height: 1;
}

.tool-button.active .tool-shortcut {
  background: rgba(255, 255, 255, 0.2);
  color: white;
}

@media (max-width: 600px) {
  .selection-toolbar {
    justify-content: center;
  }
  
  .tool-button {
    min-width: 45px;
    padding: 0.3rem 0.4rem;
  }
  
  .tool-label {
    font-size: 0.7rem;
  }
  
  .tool-icon {
    font-size: 1.1rem;
  }
}
</style>
