<!-- Footer.svelte - Bottom keyboard area -->
<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import VirtualKeyboard from './VirtualKeyboard.svelte';
  
  export let isVisible: boolean = true;
  export let showKeyboard: boolean = false;
  export let height: number = 200;
  
  const dispatch = createEventDispatcher();
    function handleKeyPress(event: CustomEvent) {
    dispatch('keypress', event.detail);
  }
    function handleToggleKeyboard() {
    dispatch('keyboardToggle');
  }
</script>

{#if isVisible}
  <footer class="footer" style="height: {height}px;">
    <div class="footer-header">
      <div class="status-info">QuickSignEditor 2025</div>
      <button 
        class="keyboard-toggle-btn" 
        on:click={handleToggleKeyboard}
        aria-label="Toggle virtual keyboard"
      >
        {showKeyboard ? '⌨️ Hide' : '⌨️ Show'} Keyboard
      </button>
    </div>
    
    {#if showKeyboard}
      <div class="keyboard-container">
        <VirtualKeyboard 
          layout="signwriting"
          on:keypress={handleKeyPress}        />
      </div>
    {/if}
  </footer>
{/if}

<style>
  .footer {
    position: fixed;
    bottom: 0;
    left: 0;
    right: 0;
    background: #f8f9fa;
    border-top: 1px solid #dee2e6;
    z-index: 1000;
    display: flex;
    flex-direction: column;
  }
  
  .footer-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 8px 16px;
    background: #e9ecef;
    border-bottom: 1px solid #dee2e6;
    min-height: 40px;
  }
  
  .status-info {
    font-weight: 600;
    color: #495057;
    font-size: 0.875rem;
  }
  
  .keyboard-toggle-btn {
    background: #007bff;
    color: white;
    border: none;
    border-radius: 4px;
    padding: 6px 12px;
    cursor: pointer;
    font-size: 0.875rem;
    font-weight: 500;
  }
  
  .keyboard-toggle-btn:hover {
    background: #0056b3;
  }
  
  .keyboard-container {
    flex: 1;
    padding: 8px;
    background: white;
    overflow-y: auto;
    max-height: 300px;
  }
  
  /* Responsive adjustments */
  @media (max-width: 768px) {
    .footer-header {
      padding: 6px 12px;
    }
      .keyboard-container {
      padding: 6px;
      max-height: 250px;
    }
  }
  
  .keyboard-row {
    display: flex;
    justify-content: center;
    gap: 2px;
  }
  
  .space-row {
    justify-content: center;
  }
  
  .key {
    background: white;
    border: 1px solid #ced4da;
    border-radius: 4px;
    padding: 8px;
    min-width: 32px;
    min-height: 32px;
    cursor: pointer;
    font-size: 12px;
    font-weight: 500;
    display: flex;
    align-items: center;
    justify-content: center;
    transition: all 0.1s ease;
  }
  
  .key:hover {
    background: #e9ecef;
    border-color: #adb5bd;
  }
  
  .key:active {
    background: #dee2e6;
    transform: translateY(1px);
  }
  
  .special-key {
    background: #f8f9fa;
    font-size: 10px;
    min-width: 48px;
  }
  
  .space-key {
    min-width: 200px;
    flex: 1;
    max-width: 300px;
  }
  
  .physical-keyboard-info {
    flex: 1;
    padding: 16px;
    display: flex;
    gap: 24px;
    align-items: flex-start;
  }
  
  .info-text {
    flex: 1;
  }
  
  .info-text p {
    margin: 4px 0;
    color: #6c757d;
  }
  
  .shortcuts {
    flex: 1;
  }
  
  .shortcuts h4 {
    margin: 0 0 8px 0;
    color: #495057;
  }
  
  .shortcuts ul {
    list-style: none;
    padding: 0;
    margin: 0;
  }
  
  .shortcuts li {
    margin: 4px 0;
    display: flex;
    align-items: center;
    gap: 8px;
    font-size: 12px;
    color: #6c757d;
  }
  
  kbd {
    background: #e9ecef;
    border: 1px solid #ced4da;
    border-radius: 3px;
    padding: 2px 6px;
    font-family: monospace;
    font-size: 11px;
    color: #495057;
  }
  
  /* Responsive adjustments */
  @media (max-width: 768px) {
    .key {
      min-width: 28px;
      min-height: 28px;
      padding: 6px;
      font-size: 11px;
    }
    
    .physical-keyboard-info {
      flex-direction: column;
      gap: 16px;
    }
    
    .shortcuts {
      flex: none;
    }
  }
  
  @media (max-width: 480px) {
    .key {
      min-width: 24px;
      min-height: 24px;
      padding: 4px;
      font-size: 10px;
    }
    
    .keyboard-header {
      padding: 6px 12px;
    }
    
    .virtual-keyboard {
      padding: 6px;
    }
  }
</style>
