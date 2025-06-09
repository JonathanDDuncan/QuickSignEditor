<script lang="ts">
  import LeftSpace from './LeftSpace.svelte';
  import CenterSpace from './CenterSpace.svelte';
  import RightSpace from './RightSpace.svelte';
  import Drawer from './Drawer.svelte';

  // Props from MainLayout
  export let containerHeight: number;
  export let windowWidth: number;
  export let isWidescreen: boolean;  export let isMediumscreen: boolean;
  export let isSmallscreen: boolean; // Passed but not used yet - reserved for future mobile features

  // Layout percentages (from Layout.State.elm)
  $: leftspacePercentage = isWidescreen ? 20 : 0;
  $: centerspacePercentage = isWidescreen ? 40 : (isMediumscreen ? 50 : 100);
  $: rightspacePercentage = isWidescreen ? 40 : (isMediumscreen ? 50 : 100);
  $: centerspaceMarginLeft = isWidescreen ? 20 : 0;
  $: rightspaceMarginLeft = isWidescreen ? 60 : (isMediumscreen ? 50 : 0);

  // Drawer state for mobile/tablet
  let drawerShowing = false;
</script>

{#if isWidescreen}
  <!-- Wide Screen Layout: Left + Center + Right -->
  <div class="widescreen-layout">
    <LeftSpace 
      {containerHeight}
      percentage={leftspacePercentage}
    />
    <CenterSpace 
      {containerHeight}
      percentage={centerspacePercentage}
      marginLeftPercentage={centerspaceMarginLeft}
    />
    <RightSpace 
      {containerHeight}
      {windowWidth}
      percentage={rightspacePercentage}
      marginLeftPercentage={rightspaceMarginLeft}
    />
  </div>
{:else if isMediumscreen}
  <!-- Medium Screen Layout: Center + Right + Drawer -->
  <div class="mediumscreen-layout">
    <CenterSpace 
      {containerHeight}
      percentage={centerspacePercentage}
      marginLeftPercentage={centerspaceMarginLeft}
    />
    <RightSpace 
      {containerHeight}
      {windowWidth}
      percentage={rightspacePercentage}
      marginLeftPercentage={rightspaceMarginLeft}
    />    <Drawer 
      isOpen={drawerShowing}
      on:toggle={() => drawerShowing = !drawerShowing}
    >
      <LeftSpace 
        {containerHeight}
        percentage={100}
      />
    </Drawer>
  </div>
{:else}
  <!-- Small Screen Layout: Center + Drawer (Left + Right) -->
  <div class="smallscreen-layout">
    <CenterSpace 
      {containerHeight}
      percentage={centerspacePercentage}
      marginLeftPercentage={centerspaceMarginLeft}
    />    <Drawer 
      isOpen={drawerShowing}
      on:toggle={() => drawerShowing = !drawerShowing}
    >
      <div class="drawer-content">
        <div class="left-section">
          <LeftSpace 
            {containerHeight}
            percentage={100}
          />
        </div>
        <div class="right-section">
          <RightSpace 
            {containerHeight}
            {windowWidth}
            percentage={100}
            marginLeftPercentage={0}
          />
        </div>
      </div>
    </Drawer>
  </div>
{/if}

<style>
  .widescreen-layout,
  .mediumscreen-layout,
  .smallscreen-layout {
    position: relative;
    width: 100%;
    height: 100%;
  }

  .drawer-content {
    display: flex;
    flex-direction: column;
    height: 100%;
  }

  .left-section {
    background-color: lightsteelblue;
    padding: 1rem;
  }

  .right-section {
    background-color: lightyellow;
    padding: 1rem;
    flex: 1;
  }
</style>
