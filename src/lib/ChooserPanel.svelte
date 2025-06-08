<script lang="ts">
// Parent chooser panel, matching legacy View.elm layout
import SymbolChooser from './SymbolChooser.svelte';
import HandGroupChooser from './HandGroupChooser.svelte';
import GeneralGroupChooser from './GeneralGroupChooser.svelte';
import Maniquin from './Maniquin.svelte';

// Props (stub for now)
export let model: any;
export let parentWidth: number = 600;
export let parentHeight: number = 400;

// Layout calculations (legacy: halfwidth, halfheight, scaling, etc.)
const halfWidth = Math.trunc(parentWidth / 2);
const halfHeight = Math.trunc(parentHeight / 2);

// Maniquin always shown (use Svelte component in markup, not as a variable)
// Remove JSX-style assignments and use Svelte blocks instead

// Main symbol chooser props (connect from model)
$: symbolChooserProps = {
  symbols: model?.symbols || [],
  filter: model?.filter || '',
  categories: model?.categories || [],
  selectedCategory: model?.selectedCategory || '',
};

// Sub-group chooser logic (connect from model)
$: isHand = model?.groupselected?.symbolkey?.startsWith('S1');

</script>

<div class="chooser-panel">
  <!-- Maniquin (hand preview) -->
  <div class="maniquin-area">
    <Maniquin {model} width={parentWidth} height={80} />
  </div>

  <!-- Main symbol chooser (left) -->
  <div class="main-chooser" style="height: {halfHeight}px; width: {halfWidth}px;">
    <div class="main-chooser-inner">
      <SymbolChooser {...symbolChooserProps} />
    </div>
  </div>

  <!-- Sub-group chooser (right) -->
  <div class="subgroup-chooser" style="height: {parentHeight}px; width: {halfWidth}px;">
    {#if isHand}
      <HandGroupChooser data={model?.handGroupData} />
    {:else}
      <GeneralGroupChooser data={model?.generalGroupData} />
    {/if}
  </div>
</div>

<style>
.chooser-panel {
  position: relative;
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: row;
}
.maniquin-area {
  /* Placeholder for hand preview */
  width: 100%;
  height: 80px;
  background: #f0f0f0;
  border-bottom: 1px solid #ccc;
}
.main-chooser {
  margin-top: 5px;
  position: relative;
}
.main-chooser-inner {
  position: relative;
  transform-origin: top left;
  /* transform: scale(...) for scaling, if needed */
  background: #fff;
  border: 1px solid #eee;
  height: 100%;
  width: 100%;
}
.subgroup-chooser {
  position: absolute;
  left: 50%;
  top: 0;
  margin-left: 0;
  height: 100%;
  width: 50%;
  overflow-y: scroll;
  overflow-x: scroll;
  background: #fafbfc;
  border-left: 1px solid #ccc;
}
</style>
