<script lang="ts">
  import { createEventDispatcher } from 'svelte';

  const dispatch = createEventDispatcher();

  function handleClose() {
    dispatch('close');
  }
</script>

<!-- Modal Overlay -->
<div 
  class="modal-overlay" 
  role="dialog" 
  aria-modal="true"
  on:click={handleClose}
  on:keydown={(e) => e.key === 'Escape' && handleClose()}
>
  <div 
    class="about-dialog" 
    role="document"
    on:click|stopPropagation
    on:keydown={(e) => e.key === 'Escape' && handleClose()}
  >
    <div class="dialog-header">
      <h2>About QuickSignEditor</h2>
      <button type="button" class="close-button" on:click={handleClose}>×</button>
    </div>
    
    <div class="dialog-content">
      <p><strong>QuickSignEditor 2025</strong></p>
      <p>A modern SignWriting editor for creating and editing sign language notation.</p>
      
      <div class="features">
        <h3>Features:</h3>
        <ul>
          <li>Interactive sign canvas with drag & drop</li>
          <li>Comprehensive symbol library</li>
          <li>FSW (Formal SignWriting) import/export</li>
          <li>Responsive design for desktop and mobile</li>
          <li>Accessibility support</li>
        </ul>
      </div>
      
      <div class="credits">
        <h3>Credits:</h3>
        <p>Based on the original Elm implementation</p>
        <p>Rewritten in Svelte 3 for modern web standards</p>
      </div>
    </div>
    
    <div class="dialog-footer">
      <button type="button" class="ok-button" on:click={handleClose}>OK</button>
    </div>
  </div>
</div>

<style>
  .modal-overlay {
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: rgba(0, 0, 0, 0.5);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 1000;
  }

  .about-dialog {
    position: relative;
    background: white;
    border-radius: 8px;
    max-width: 500px;
    width: 90%;
    max-height: 80vh;
    overflow: auto;
    box-shadow: 0 4px 20px rgba(0, 0, 0, 0.3);
  }

  .dialog-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 1rem 1.5rem;
    border-bottom: 1px solid #eee;
  }

  .dialog-header h2 {
    margin: 0;
    color: #333;
  }

  .close-button {
    background: none;
    border: none;
    font-size: 24px;
    cursor: pointer;
    color: #666;
    line-height: 1;
  }

  .close-button:hover {
    color: #333;
  }

  .dialog-content {
    padding: 1.5rem;
  }

  .dialog-content p {
    margin: 0 0 1rem 0;
    line-height: 1.5;
  }

  .features, .credits {
    margin-top: 1.5rem;
  }

  .features h3, .credits h3 {
    margin: 0 0 0.5rem 0;
    color: #333;
    font-size: 1.1em;
  }

  .features ul {
    margin: 0;
    padding-left: 1.5rem;
  }

  .features li {
    margin-bottom: 0.25rem;
  }

  .dialog-footer {
    padding: 1rem 1.5rem;
    border-top: 1px solid #eee;
    text-align: right;
  }

  .ok-button {
    background: #2196F3;
    color: white;
    border: none;
    border-radius: 4px;
    padding: 0.5rem 1.5rem;
    cursor: pointer;
    font-size: 14px;
    font-weight: 500;
    text-transform: uppercase;
  }

  .ok-button:hover {
    background: #1976D2;
  }
</style>
