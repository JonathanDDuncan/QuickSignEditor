<script lang="ts">
  import { createEventDispatcher } from 'svelte';

  // Props
  export let containerHeight: number;
  export let percentage: number;

  const dispatch = createEventDispatcher();

  // Button handlers (from LeftSpace.elm functionality)
  function handleSave() {
    dispatch('save');
  }

  function handleCancel() {
    dispatch('cancel');
  }

  function handleAbout() {
    dispatch('about');
  }

  function handleShareFsw() {
    dispatch('shareFsw');
  }
</script>

<div 
  class="leftspace" 
  style="height: {containerHeight}px; width: {percentage}%;"
>
  <!-- Action Buttons Grid (matching Material Design from Elm) -->
  <div class="button-grid">
    <div class="button-row">
      <button 
        type="button"
        class="action-button primary"
        on:click={handleSave}
      >
        Save
      </button>
      <button 
        type="button"
        class="action-button secondary"
        on:click={handleCancel}
      >
        Cancel
      </button>
    </div>
  </div>

  <!-- Bottom Section (matching Elm layout) -->
  <div class="bottom-section">
    <div class="bottom-actions">
      <button 
        type="button"
        class="action-button accent"
        on:click={handleShareFsw}
      >
        Share FSW
      </button>
    </div>
    
    <!-- About Button -->
    <div class="about-section">
      <button 
        type="button"
        class="about-button"
        on:click={handleAbout}
      >
        About ...
      </button>
    </div>
  </div>
</div>

<style>
  .leftspace {
    background-color: lightsteelblue;
    position: fixed;
    top: 0;
    left: 0;
    z-index: 10;
    display: flex;
    flex-direction: column;
    box-sizing: border-box;
    padding: 8px;
    padding-top: 4px;
    color: white;
  }

  .button-grid {
    flex: 1;
    display: flex;
    flex-direction: column;
    justify-content: flex-start;
  }

  .button-row {
    display: flex;
    gap: 8px;
    margin-bottom: 8px;
  }

  .action-button {
    padding: 8px 16px;
    border: none;
    border-radius: 4px;
    cursor: pointer;
    font-size: 14px;
    font-weight: 500;
    text-transform: uppercase;
    transition: all 0.2s ease;
    flex: 1;
  }

  .action-button.primary {
    background-color: #2196F3;
    color: white;
  }

  .action-button.primary:hover {
    background-color: #1976D2;
  }

  .action-button.secondary {
    background-color: #FF9800;
    color: white;
  }

  .action-button.secondary:hover {
    background-color: #F57C00;
  }

  .action-button.accent {
    background-color: #FF5722;
    color: white;
  }

  .action-button.accent:hover {
    background-color: #D84315;
  }

  .bottom-section {
    position: absolute;
    bottom: 0;
    left: 0;
    right: 0;
    padding: 8px;
  }

  .bottom-actions {
    text-align: center;
    margin-bottom: 20px;
  }

  .about-section {
    text-align: center;
  }

  .about-button {
    background: none;
    border: 1px solid white;
    color: white;
    padding: 8px 16px;
    border-radius: 4px;
    cursor: pointer;
    font-size: 14px;
  }

  .about-button:hover {
    background-color: rgba(255, 255, 255, 0.1);
  }
</style>
