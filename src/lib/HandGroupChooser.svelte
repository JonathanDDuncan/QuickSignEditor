<script lang="ts">
// Hand group chooser stub (for legacy parity)
export let data: any;
</script>

<div class="hand-group-chooser">
  {#if data?.label}
    <div class="group-label">{data.label}</div>
  {/if}
  {#if data?.items && data.items.length > 0}
    <ul class="group-items">
      {#each data.items as item}
        <li class="group-item">Hand Group Item: {item}</li>
      {/each}
    </ul>
  {:else}
    <div class="placeholder">No hand group items.</div>
  {/if}
</div>

<style>
.hand-group-chooser {
  padding: 12px;
  background: #f8f8ff;
  border-radius: 4px;
  min-height: 80px;
}
.placeholder {
  color: #888;
  font-size: 1em;
}
.group-label {
  font-weight: bold;
  margin-bottom: 6px;
}
.group-items {
  list-style: none;
  padding: 0;
  margin: 0 0 0 8px;
}
.group-item {
  padding: 2px 0 2px 8px;
  font-size: 0.97em;
  color: #333;
}
</style>
