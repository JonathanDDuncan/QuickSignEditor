<!-- Drawer.svelte - Mobile/tablet navigation drawer -->
<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import { slide } from 'svelte/transition';
  
  export let isOpen: boolean = false;
  export let position: 'left' | 'right' = 'left';
  export let width: number = 280;
  export let overlay: boolean = true;
  
  const dispatch = createEventDispatcher();
  
  // Navigation menu items
  const menuItems = [
    { id: 'new', label: 'New Sign', icon: '📝', shortcut: 'Ctrl+N' },
    { id: 'open', label: 'Open Sign', icon: '📂', shortcut: 'Ctrl+O' },
    { id: 'save', label: 'Save Sign', icon: '💾', shortcut: 'Ctrl+S' },
    { id: 'saveas', label: 'Save As...', icon: '📄', shortcut: 'Ctrl+Shift+S' },
    { id: 'separator1', type: 'separator' },
    { id: 'export-fsw', label: 'Share FSW', icon: '🔗', shortcut: '' },
    { id: 'export-svg', label: 'Export SVG', icon: '🖼️', shortcut: '' },
    { id: 'export-png', label: 'Export PNG', icon: '🖼️', shortcut: '' },
    { id: 'separator2', type: 'separator' },
    { id: 'undo', label: 'Undo', icon: '↶', shortcut: 'Ctrl+Z' },
    { id: 'redo', label: 'Redo', icon: '↷', shortcut: 'Ctrl+Y' },
    { id: 'separator3', type: 'separator' },
    { id: 'settings', label: 'Settings', icon: '⚙️', shortcut: '' },
    { id: 'about', label: 'About', icon: 'ℹ️', shortcut: '' },
    { id: 'help', label: 'Help', icon: '❓', shortcut: 'F1' }
  ];
  
  function handleMenuClick(item: any) {
    if (item.type === 'separator') return;
    
    dispatch('menuAction', { action: item.id, label: item.label });
    
    // Close drawer on mobile after action
    if (window.innerWidth < 768) {
      handleClose();
    }
  }
  
  function handleClose() {
    dispatch('close');
  }
  
  function handleOverlayClick() {
    if (overlay) {
      handleClose();
    }
  }
  
  // Handle escape key
  function handleKeydown(event: KeyboardEvent) {
    if (event.key === 'Escape' && isOpen) {
      handleClose();
    }
  }
</script>

<svelte:window on:keydown={handleKeydown} />

{#if isOpen}
  <!-- Overlay -->  {#if overlay}
    <div
      class="drawer-overlay"
      role="button"
      tabindex="0"
      on:click={handleOverlayClick}
      on:keydown={(e) => (e.key === 'Enter' || e.key === ' ') && handleOverlayClick()}
      transition:slide={{ duration: 200 }}
    ></div>
  {/if}
  
  <!-- Drawer -->
  <nav 
    class="drawer"
    class:drawer-left={position === 'left'}
    class:drawer-right={position === 'right'}
    style="width: {width}px;"
    transition:slide={{ duration: 300, axis: 'x' }}
  >
    <!-- Header -->
    <div class="drawer-header">
      <h3 class="drawer-title">QuickSignEditor</h3>
      <button class="close-btn" on:click={handleClose} aria-label="Close menu">
        ✕
      </button>
    </div>
    
    <!-- Menu Items -->
    <div class="drawer-content">
      <ul class="menu-list">
        {#each menuItems as item}
          {#if item.type === 'separator'}
            <li class="menu-separator"></li>
          {:else}
            <li class="menu-item">
              <button 
                class="menu-button"
                on:click={() => handleMenuClick(item)}
                title={item.shortcut}
              >
                <span class="menu-icon">{item.icon}</span>
                <span class="menu-label">{item.label}</span>
                {#if item.shortcut}
                  <span class="menu-shortcut">{item.shortcut}</span>
                {/if}
              </button>
            </li>
          {/if}
        {/each}
      </ul>
    </div>
    
    <!-- Footer -->
    <div class="drawer-footer">
      <div class="app-info">
        <div class="version">v1.0.0</div>
        <div class="build">SignWriting Editor</div>
      </div>
    </div>
  </nav>
{/if}

<style>
  .drawer-overlay {
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: rgba(0, 0, 0, 0.5);
    z-index: 1998;
  }
  
  .drawer {
    position: fixed;
    top: 0;
    bottom: 0;
    background: white;
    box-shadow: 2px 0 8px rgba(0, 0, 0, 0.15);
    z-index: 1999;
    display: flex;
    flex-direction: column;
    max-width: 90vw;
  }
  
  .drawer-left {
    left: 0;
  }
  
  .drawer-right {
    right: 0;
    box-shadow: -2px 0 8px rgba(0, 0, 0, 0.15);
  }
  
  .drawer-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 16px 20px;
    border-bottom: 1px solid #e9ecef;
    background: #f8f9fa;
  }
  
  .drawer-title {
    margin: 0;
    font-size: 18px;
    font-weight: 600;
    color: #212529;
  }
  
  .close-btn {
    background: none;
    border: none;
    font-size: 18px;
    color: #6c757d;
    cursor: pointer;
    padding: 4px;
    border-radius: 4px;
    display: flex;
    align-items: center;
    justify-content: center;
    width: 32px;
    height: 32px;
  }
  
  .close-btn:hover {
    background: #e9ecef;
    color: #495057;
  }
  
  .drawer-content {
    flex: 1;
    overflow-y: auto;
    padding: 8px 0;
  }
  
  .menu-list {
    list-style: none;
    margin: 0;
    padding: 0;
  }
  
  .menu-item {
    margin: 0;
  }
  
  .menu-button {
    width: 100%;
    background: none;
    border: none;
    text-align: left;
    padding: 12px 20px;
    cursor: pointer;
    display: flex;
    align-items: center;
    gap: 12px;
    font-size: 14px;
    color: #495057;
    transition: background-color 0.15s ease;
  }
  
  .menu-button:hover {
    background: #f8f9fa;
    color: #212529;
  }
  
  .menu-button:active {
    background: #e9ecef;
  }
  
  .menu-icon {
    font-size: 16px;
    width: 20px;
    text-align: center;
    flex-shrink: 0;
  }
  
  .menu-label {
    flex: 1;
    font-weight: 500;
  }
  
  .menu-shortcut {
    font-size: 11px;
    color: #6c757d;
    background: #f8f9fa;
    padding: 2px 6px;
    border-radius: 3px;
    font-family: monospace;
    flex-shrink: 0;
  }
  
  .menu-separator {
    height: 1px;
    background: #e9ecef;
    margin: 8px 20px;
  }
  
  .drawer-footer {
    border-top: 1px solid #e9ecef;
    padding: 16px 20px;
    background: #f8f9fa;
  }
  
  .app-info {
    text-align: center;
  }
  
  .version {
    font-size: 12px;
    color: #6c757d;
    font-weight: 600;
  }
  
  .build {
    font-size: 11px;
    color: #adb5bd;
    margin-top: 2px;
  }
  
  /* Responsive adjustments */
  @media (min-width: 768px) {
    .drawer {
      position: relative;
      box-shadow: none;
      border-right: 1px solid #e9ecef;
    }
    
    .drawer-overlay {
      display: none;
    }
  }
  
  @media (max-width: 480px) {
    .drawer {
      width: 85vw !important;
      max-width: none;
    }
    
    .drawer-header {
      padding: 12px 16px;
    }
    
    .drawer-title {
      font-size: 16px;
    }
    
    .menu-button {
      padding: 10px 16px;
      font-size: 13px;
    }
    
    .menu-icon {
      font-size: 14px;
      width: 18px;
    }
    
    .drawer-footer {
      padding: 12px 16px;
    }
  }
</style>
