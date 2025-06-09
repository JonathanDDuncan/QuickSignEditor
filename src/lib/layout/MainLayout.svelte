<script lang="ts">
  import { onMount } from 'svelte';
  import ScreenLayout from './ScreenLayout.svelte';
  import AboutDialog from './AboutDialog.svelte';
  import Footer from './Footer.svelte';
  import Drawer from './Drawer.svelte';

  // Props
  export let showAboutDialog = false;
  export let showDrawer = false;
  export let showKeyboard = false;

  // Responsive breakpoints (matching Elm implementation)
  let windowWidth = 1000;
  let windowHeight = 800;
  
  // Layout constants from Layout.State.elm
  const widescreenWidth = 1000;
  const mediumscreenWidth = 600;
  const footerHeight = 176;
  
  // Computed layout properties
  $: containerHeight = windowHeight - footerHeight;
  $: isWidescreen = windowWidth > widescreenWidth;
  $: isMediumscreen = windowWidth > mediumscreenWidth && windowWidth <= widescreenWidth;
  $: isSmallscreen = windowWidth <= mediumscreenWidth;

  // Window resize handler
  function handleResize() {
    windowWidth = window.innerWidth;
    windowHeight = window.innerHeight;
  }
  onMount(() => {
    handleResize();
    window.addEventListener('resize', handleResize);
    return () => window.removeEventListener('resize', handleResize);
  });

  // Layout state handlers
  function handleAboutClose() {
    showAboutDialog = false;
  }
  
  function handleDrawerToggle() {
    showDrawer = !showDrawer;
  }
  
  function handleKeyboardToggle() {
    showKeyboard = !showKeyboard;
  }
</script>

<div class="layout-container">
  <!-- About Dialog Modal -->
  {#if showAboutDialog}
    <AboutDialog on:close={handleAboutClose} />
  {/if}

  <!-- Mobile/Tablet Drawer -->
  {#if isSmallscreen || isMediumscreen}
    <Drawer 
      isOpen={showDrawer} 
      on:close={() => showDrawer = false}
      on:toggle={handleDrawerToggle}
    />
  {/if}

  <!-- Main Content Area -->
  <div class="main-content">    <ScreenLayout 
      {containerHeight}
      {windowWidth}
      {isWidescreen}
      {isMediumscreen}
      {isSmallscreen}
    />
  </div>

  <!-- Footer with Keyboard -->
  <Footer 
    height={footerHeight}
    {showKeyboard}
    on:keyboardToggle={handleKeyboardToggle}
  />
</div>

<style>
  .layout-container {
    display: flex;
    flex-direction: column;
    height: 100vh;
    width: 100vw;
    overflow: hidden;
  }

  .main-content {
    flex: 1;
    position: relative;
    overflow: hidden;
  }
</style>
