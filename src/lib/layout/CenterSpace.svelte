<script lang="ts">
  import SignEditor from '../SignEditor.svelte';
  import Toolbar from '../Toolbar.svelte';
  import VirtualKeyboard from './VirtualKeyboard.svelte';

  // Props
  export let containerHeight: number;
  export let percentage: number;
  export let marginLeftPercentage: number;
  export let showKeyboard = false;

  // Sign editor state (will be connected to stores later)
  let signData = {};
  
  // Editor toolbar actions
  function handleToolbarAction(event) {
    console.log('Toolbar action:', event.detail);
  }
  
  // Keyboard input handler
  function handleKeyboardInput(event) {
    console.log('Keyboard input:', event.detail);
  }
</script>

<div 
  class="centerspace" 
  style="height: {containerHeight}px; width: {percentage}%; margin-left: {marginLeftPercentage}%;"
>
  <!-- Editor Toolbar (commands view from SWEditor) -->
  <div class="toolbar-container">
    <Toolbar on:action={handleToolbarAction} />
  </div>
  
  <!-- SignWriting Editor Canvas (sign view from SWEditor) -->
  <div class="editor-container">
    <SignEditor {signData} />
    
    <!-- Selection and Rectangle Tools would go here -->
    <div class="editor-tools">
      <!-- RectangleSelect.svelte will go here -->
      <div class="tool-placeholder">Rectangle Select Tool</div>
    </div>
  </div>
  
  <!-- Virtual Keyboard (when shown in center space) -->
  {#if showKeyboard}
    <div class="keyboard-container">
      <VirtualKeyboard 
        layout="signwriting"
        on:keypress={handleKeyboardInput}
      />
    </div>
  {/if}
</div>

<style>
  .centerspace {
    background-color: lightyellow;
    position: fixed;
    top: 0;
    z-index: 10;
    display: flex;
    flex-direction: column;
    border: 2px solid orange;
    border-radius: 4px;
    padding: 8px;
  }
  
  .toolbar-container {
    flex-shrink: 0;
    background: rgba(255, 255, 255, 0.9);
    border-radius: 4px;
    margin-bottom: 8px;
    padding: 4px;
  }
  
  .editor-container {
    flex: 1;
    display: flex;
    flex-direction: column;
    position: relative;
    background: white;
    border-radius: 4px;
    overflow: hidden;
  }
  
  .editor-tools {
    position: absolute;
    top: 8px;
    right: 8px;
    background: rgba(0, 0, 0, 0.7);
    color: white;
    padding: 4px 8px;
    border-radius: 4px;
    font-size: 0.75rem;
    z-index: 20;
  }
  
  .tool-placeholder {
    font-style: italic;
  }
  
  .keyboard-container {
    flex-shrink: 0;
    background: rgba(255, 255, 255, 0.95);
    border-radius: 4px;
    margin-top: 8px;
    padding: 8px;
    border: 1px solid #ddd;
  }
  
  /* Responsive adjustments */
  @media (max-width: 768px) {
    .centerspace {
      padding: 4px;
    }
    
    .toolbar-container,
    .keyboard-container {
      margin: 4px 0;
      padding: 4px;
    }
  }
</style>
