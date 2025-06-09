<script lang="ts">
  import ChooserPanel from '../ChooserPanel.svelte';

  // Props
  export let containerHeight: number;
  export let windowWidth: number;
  export let percentage: number;
  export let marginLeftPercentage: number;

  // Calculate right space width (from RightSpace.elm)
  $: rightspaceWidth = Math.trunc((100 - marginLeftPercentage) * windowWidth / 100);

  // Chooser model (will be connected to stores later)
  let chooserModel = {
    symbols: [],
    filter: '',
    categories: [],
    selectedCategory: ''
  };
</script>

<div 
  class="rightspace" 
  style="height: {containerHeight}px; width: {percentage}%; margin-left: {marginLeftPercentage}%;"
>
  <!-- Choosers Panel (from Choosers.View.elm) -->
  <ChooserPanel 
    model={chooserModel}
    parentWidth={rightspaceWidth}
    parentHeight={containerHeight}
  />
</div>

<style>
  .rightspace {
    background-color: lightyellow;
    position: fixed;
    top: 0;
    padding-right: 50px;
    z-index: 2;
  }
</style>
