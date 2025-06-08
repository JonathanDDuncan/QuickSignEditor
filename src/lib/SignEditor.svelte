<script lang="ts">
  import { validateSignData } from './utils/validation';
  import type { SignData } from './utils/types';

  export let signData: SignData = { name: '', symbols: [] };

  $: validation = validateSignData(signData);
</script>

{#if !validation.isValid}
  <div class="error">{validation.error}</div>
{/if}

<!-- Placeholder for canvas and symbol rendering -->
<div class="sign-canvas" role="region" aria-label="Sign canvas">
  <!-- TODO: Render symbols here -->
  <p>SignEditor canvas coming soon...</p>
</div>

<style>
.sign-canvas {
  width: 500px;
  height: 500px;
  border: 1px solid #ccc;
  margin: 1rem 0;
  background: #fff;
  display: flex;
  align-items: center;
  justify-content: center;
}
.error {
  color: #b00;
  margin-bottom: 0.5rem;
}
</style>
