<!-- VirtualKeyboard.svelte - On-screen keyboard component (replaces Keyboard.View.elm) -->
<script>
  import { createEventDispatcher } from 'svelte';
  
  const dispatch = createEventDispatcher();
  
  // Props
  export let layout = 'qwerty';
  export let isVisible = true;
  export let compact = false;
  
  // Keyboard layouts
  const layouts = {
    qwerty: [
      ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', '⌫'],
      ['Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '[', ']'],
      ['A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ';', '\'', '↵'],
      ['⇧', 'Z', 'X', 'C', 'V', 'B', 'N', 'M', ',', '.', '/', '⇧'],
      ['Ctrl', 'Alt', '␣', 'Alt', 'Ctrl']
    ],
    signwriting: [
      // SignWriting specific keys could go here
      ['FSW', 'SWU', 'Copy', 'Paste', 'Clear'],
      ['←', '→', '↑', '↓', 'Rotate', 'Color', 'Size'],
      ['Undo', 'Redo', 'Save', 'Load', 'Export']
    ]
  };
  
  $: currentLayout = layouts[layout] || layouts.qwerty;
  
  // Key press handler
  function handleKeyPress(key) {
    dispatch('keypress', { key });
    
    // Handle special keys
    switch(key) {
      case '⌫':
        dispatch('backspace');
        break;
      case '↵':
        dispatch('enter');
        break;
      case '⇧':
        dispatch('shift');
        break;
      case '␣':
        dispatch('space');
        break;
      case 'Ctrl':
        dispatch('ctrl');
        break;
      case 'Alt':
        dispatch('alt');
        break;
      default:
        dispatch('character', { character: key });
    }
  }
</script>

{#if isVisible}
  <div class="virtual-keyboard" class:compact>
    <div class="keyboard-header">
      <span class="keyboard-title">Virtual Keyboard ({layout})</span>
      <button 
        class="layout-switch"
        on:click={() => layout = layout === 'qwerty' ? 'signwriting' : 'qwerty'}
        aria-label="Switch keyboard layout"
      >
        {layout === 'qwerty' ? 'SW' : 'ABC'}
      </button>
    </div>
    
    <div class="keyboard-rows">
      {#each currentLayout as row, rowIndex}
        <div class="keyboard-row" data-row={rowIndex}>
          {#each row as key}
            <button
              class="keyboard-key"
              class:special={['⌫', '↵', '⇧', 'Ctrl', 'Alt'].includes(key)}
              class:space={key === '␣'}
              on:click={() => handleKeyPress(key)}
              aria-label={key === '␣' ? 'Space' : key}
            >
              {key === '␣' ? 'Space' : key}
            </button>
          {/each}
        </div>
      {/each}
    </div>
  </div>
{/if}

<style>
  .virtual-keyboard {
    background: var(--keyboard-bg, #f8f9fa);
    border: 1px solid var(--border-color, #dee2e6);
    border-radius: 8px;
    padding: 1rem;
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
    max-width: 100%;
    overflow: hidden;
  }
  
  .virtual-keyboard.compact {
    padding: 0.5rem;
    border-radius: 4px;
  }
  
  .keyboard-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 0.75rem;
    padding-bottom: 0.5rem;
    border-bottom: 1px solid var(--border-color, #dee2e6);
  }
  
  .keyboard-title {
    font-weight: 600;
    color: var(--text-primary, #212529);
    font-size: 0.875rem;
  }
  
  .layout-switch {
    background: var(--button-secondary, #6c757d);
    color: white;
    border: none;
    padding: 0.25rem 0.5rem;
    border-radius: 4px;
    cursor: pointer;
    font-size: 0.75rem;
    font-weight: 600;
  }
  
  .layout-switch:hover {
    background: var(--button-secondary-hover, #5a6268);
  }
  
  .keyboard-rows {
    display: flex;
    flex-direction: column;
    gap: 0.375rem;
  }
  
  .keyboard-row {
    display: flex;
    gap: 0.25rem;
    justify-content: center;
  }
  
  .keyboard-row[data-row="4"] {
    /* Space bar row - different layout */
    justify-content: space-evenly;
  }
  
  .keyboard-key {
    background: var(--key-bg, #ffffff);
    border: 1px solid var(--key-border, #ced4da);
    border-radius: 4px;
    padding: 0.5rem 0.75rem;
    cursor: pointer;
    font-family: inherit;
    font-size: 0.875rem;
    font-weight: 500;
    color: var(--text-primary, #212529);
    transition: all 0.15s ease;
    min-width: 2.5rem;
    height: 2.5rem;
    display: flex;
    align-items: center;
    justify-content: center;
  }
  
  .keyboard-key:hover {
    background: var(--key-hover, #e9ecef);
    border-color: var(--key-border-hover, #adb5bd);
  }
  
  .keyboard-key:active {
    background: var(--key-active, #dee2e6);
    transform: translateY(1px);
  }
  
  .keyboard-key.special {
    background: var(--key-special-bg, #007bff);
    color: white;
    font-weight: 600;
  }
  
  .keyboard-key.special:hover {
    background: var(--key-special-hover, #0056b3);
  }
  
  .keyboard-key.space {
    flex: 1;
    max-width: 12rem;
    min-width: 8rem;
  }
  
  /* Compact mode adjustments */
  .compact .keyboard-key {
    padding: 0.375rem 0.5rem;
    min-width: 2rem;
    height: 2rem;
    font-size: 0.75rem;
  }
  
  .compact .keyboard-rows {
    gap: 0.25rem;
  }
  
  .compact .keyboard-row {
    gap: 0.1875rem;
  }
  
  /* Responsive design */
  @media (max-width: 768px) {
    .virtual-keyboard {
      padding: 0.5rem;
    }
    
    .keyboard-key {
      padding: 0.375rem 0.5rem;
      min-width: 2rem;
      height: 2rem;
      font-size: 0.75rem;
    }
    
    .keyboard-rows {
      gap: 0.25rem;
    }
    
    .keyboard-row {
      gap: 0.1875rem;
    }
  }
  
  @media (max-width: 480px) {
    .keyboard-key {
      padding: 0.25rem 0.375rem;
      min-width: 1.75rem;
      height: 1.75rem;
      font-size: 0.6875rem;
    }
  }
</style>
