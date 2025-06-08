<script lang="ts">
// Maniquin (hand preview) stub
export let model: any;
export let width: number = 200;
export let height: number = 80;
</script>

<div class="maniquin">
  <!-- TODO: Render hand preview based on model -->
  <div class="placeholder">Maniquin Preview (stub)</div>
</div>

<style>
.maniquin {
  width: 100%;
  height: 100%;
  background: #f0f0f0;
  display: flex;
  align-items: center;
  justify-content: center;
  border-bottom: 1px solid #ccc;
}
.placeholder {
  color: #888;
  font-size: 1em;
}
</style>
