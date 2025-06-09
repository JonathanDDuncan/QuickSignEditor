# Svelte Implementation Guide - From Elm to Svelte

## Project Setup

### 1. Initialize Svelte Project Structure

```bash
# Create the recommended directory structure
mkdir -p src/lib/{stores,types,utils,signwriting,components/{layout,editor,choosers,keyboard,common}}
mkdir -p src/routes
```

### 2. Core Store Setup

Create the main application stores that replace Elm's state management:

#### `src/lib/stores/appState.js`
```javascript
import { writable, derived } from 'svelte/store';

// Global application state
export const appState = writable({
  isInitialized: false,
  currentMode: 'editor', // 'editor' | 'chooser' | 'keyboard'
  errors: [],
  loading: false
});

// Global UI state
export const uiState = writable({
  showOverlay: false,
  showAboutDialog: false,
  theme: 'default'
});
```

#### `src/lib/stores/layoutState.js`
```javascript
import { writable, derived } from 'svelte/store';

// Window size and responsive state
export const windowSize = writable({
  width: 1024,
  height: 768
});

// Layout configuration
export const layoutConfig = writable({
  widescreenWidth: 1000,
  mediumscreenWidth: 600,
  rightspacePercentage: 50,
  leftspacePercentage: 25,
  centerspacePercentage: 25
});

// Derived responsive state
export const screenType = derived(
  [windowSize, layoutConfig],
  ([$windowSize, $layoutConfig]) => {
    if ($windowSize.width > $layoutConfig.widescreenWidth) return 'wide';
    if ($windowSize.width > $layoutConfig.mediumscreenWidth) return 'medium';
    return 'small';
  }
);

// Drawer state for mobile
export const drawerState = writable({
  isOpen: false,
  content: null
});
```

#### `src/lib/stores/editorState.js`
```javascript
import { writable, derived } from 'svelte/store';

// Sign data
export const currentSign = writable({
  symbols: [],
  width: 300,
  height: 300
});

// Editor mode and tools
export const editorMode = writable('normal'); // 'normal' | 'rectangleSelect' | 'drag'

// Selection state
export const selectedSymbols = writable([]);

// Undo/redo stacks
export const undoStack = writable([]);
export const redoStack = writable([]);

// Editor UI state
export const editorUI = writable({
  zoom: 1.0,
  panX: 0,
  panY: 0,
  showGrid: false,
  snapToGrid: true
});
```

### 3. Component Implementation Examples

#### `src/App.svelte` (Application Root)
```svelte
<script>
  import { onMount } from 'svelte';
  import { appState, uiState } from './lib/stores/appState.js';
  import { windowSize } from './lib/stores/layoutState.js';
  import MainLayout from './lib/components/layout/MainLayout.svelte';
  import AboutDialog from './lib/components/layout/AboutDialog.svelte';

  // Handle window resize
  function handleResize() {
    windowSize.set({
      width: window.innerWidth,
      height: window.innerHeight
    });
  }

  onMount(() => {
    handleResize();
    window.addEventListener('resize', handleResize);
    
    // Initialize application
    appState.update(state => ({
      ...state,
      isInitialized: true
    }));

    return () => {
      window.removeEventListener('resize', handleResize);
    };
  });
</script>

<svelte:window on:resize={handleResize} />

<main>
  <MainLayout />
  {#if $uiState.showAboutDialog}
    <AboutDialog />
  {/if}
</main>

<style>
  main {
    width: 100%;
    height: 100vh;
    overflow: hidden;
  }
</style>
```

#### `src/lib/components/layout/MainLayout.svelte`
```svelte
<script>
  import { screenType } from '../../stores/layoutState.js';
  import CenterSpace from './CenterSpace.svelte';
  import LeftSpace from './LeftSpace.svelte';
  import RightSpace from './RightSpace.svelte';
  import Footer from './Footer.svelte';
  import Drawer from './Drawer.svelte';
  import AboutDialog from './AboutDialog.svelte';

  $: isWideScreen = $screenType === 'wide';
  $: isMediumScreen = $screenType === 'medium';
  $: isSmallScreen = $screenType === 'small';
</script>

<div class="layout-container">
  <AboutDialog />
  
  <div class="main-content">
    {#if isWideScreen}
      <div class="wide-layout">
        <LeftSpace />
        <CenterSpace />
        <RightSpace />
      </div>
    {:else if isMediumScreen}
      <div class="medium-layout">
        <CenterSpace />
        <RightSpace />
        <Drawer>
          <LeftSpace />
        </Drawer>
      </div>
    {:else}
      <div class="small-layout">
        <CenterSpace />
        <Drawer>
          <div class="drawer-content">
            <div class="left-section">
              <LeftSpace />
            </div>
            <div class="right-section">
              <RightSpace />
            </div>
          </div>
        </Drawer>
      </div>
    {/if}
  </div>
  
  <Footer />
</div>

<style>
  .layout-container {
    display: flex;
    flex-direction: column;
    height: 100vh;
  }

  .main-content {
    flex: 1;
    overflow: hidden;
  }

  .wide-layout {
    display: flex;
    height: 100%;
  }

  .medium-layout {
    display: flex;
    height: 100%;
  }

  .small-layout {
    height: 100%;
  }

  .drawer-content {
    display: flex;
    flex-direction: column;
  }

  .left-section {
    background-color: lightsteelblue;
    padding: 1rem;
  }

  .right-section {
    background-color: lightyellow;
    padding: 1rem;
  }
</style>
```

#### `src/lib/components/choosers/Mannequin.svelte` (Key Component Migration)
```svelte
<script>
  import { createEventDispatcher } from 'svelte';
  import { chooserState } from '../../stores/chooserState.js';
  
  export let width = 200;
  export let height = 300;
  export let model;

  const dispatch = createEventDispatcher();

  // Mannequin body parts and their click areas
  const bodyParts = [
    { id: 'head', x: 85, y: 10, width: 30, height: 30 },
    { id: 'torso', x: 75, y: 45, width: 50, height: 80 },
    { id: 'leftArm', x: 35, y: 50, width: 35, height: 60 },
    { id: 'rightArm', x: 130, y: 50, width: 35, height: 60 },
    { id: 'leftLeg', x: 65, y: 130, width: 25, height: 100 },
    { id: 'rightLeg', x: 110, y: 130, width: 25, height: 100 }
  ];

  function handleBodyPartClick(bodyPart) {
    dispatch('bodyPartSelected', {
      part: bodyPart.id,
      position: { x: bodyPart.x, y: bodyPart.y }
    });
  }

  // Convert from Elm mannequin logic
  function getMannequinData(model, width, height) {
    return {
      width,
      height,
      divheight: height + 20, // Account for margin
      view: null // Will be the SVG content
    };
  }

  $: mannequinData = getMannequinData(model, width, height);
</script>

<div class="mannequin-container" style="width: {width}px; height: {height}px;">
  <svg {width} {height} viewBox="0 0 200 300">
    <!-- Basic mannequin outline -->
    <g class="mannequin-outline">
      <!-- Head -->
      <circle cx="100" cy="25" r="15" class="body-part" />
      
      <!-- Torso -->
      <rect x="75" y="45" width="50" height="80" rx="10" class="body-part" />
      
      <!-- Arms -->
      <rect x="35" y="50" width="35" height="60" rx="15" class="body-part" />
      <rect x="130" y="50" width="35" height="60" rx="15" class="body-part" />
      
      <!-- Legs -->
      <rect x="65" y="130" width="25" height="100" rx="10" class="body-part" />
      <rect x="110" y="130" width="25" height="100" rx="10" class="body-part" />
    </g>
    
    <!-- Clickable areas -->
    {#each bodyParts as bodyPart}
      <rect
        x={bodyPart.x}
        y={bodyPart.y}
        width={bodyPart.width}
        height={bodyPart.height}
        class="click-area"
        on:click={() => handleBodyPartClick(bodyPart)}
        role="button"
        tabindex="0"
        aria-label="Select {bodyPart.id}"
      />
    {/each}
  </svg>
</div>

<style>
  .mannequin-container {
    display: inline-block;
    margin: 5px;
  }

  .body-part {
    fill: #e0e0e0;
    stroke: #333;
    stroke-width: 2;
  }

  .click-area {
    fill: transparent;
    cursor: pointer;
  }

  .click-area:hover {
    fill: rgba(0, 0, 255, 0.1);
  }
</style>
```

#### `src/lib/components/editor/SignEditor.svelte`
```svelte
<script>
  import { currentSign, editorMode, selectedSymbols } from '../../stores/editorState.js';
  import EditorToolbar from './EditorToolbar.svelte';
  import SignCanvas from './SignCanvas.svelte';
  import RectangleSelect from './RectangleSelect.svelte';

  export let containerHeight = 500;
  export let signViewMargin = 10;

  $: maxWidthHeight = 575;
  $: canvasSize = Math.min(containerHeight - 50, maxWidthHeight);
</script>

<div class="sign-editor">
  <EditorToolbar />
  
  <div class="editor-content">
    <SignCanvas 
      width={canvasSize}
      height={canvasSize}
      margin={signViewMargin}
      sign={$currentSign}
      selectedSymbols={$selectedSymbols}
      on:symbolSelected
      on:symbolMoved
      on:symbolDeleted
    />
    
    {#if $editorMode === 'rectangleSelect'}
      <RectangleSelect />
    {/if}
  </div>
</div>

<style>
  .sign-editor {
    display: flex;
    height: 100%;
  }

  .editor-content {
    position: relative;
    flex: 1;
  }
</style>
```

### 4. Type Definitions

#### `src/lib/types/signwriting.ts`
```typescript
export interface Symbol {
  id: string;
  key: string;
  x: number;
  y: number;
  fill: number;
  rotation: number;
  size: number;
}

export interface Sign {
  symbols: Symbol[];
  width: number;
  height: number;
}

export interface SymbolGroup {
  category: string;
  symbols: string[];
}

export interface MannequinBodyPart {
  id: string;
  x: number;
  y: number;
  width: number;
  height: number;
}
```

### 5. Utility Functions Migration

#### `src/lib/utils/viewHelpers.ts`
```typescript
// Migrate from Helpers.ViewExtra.elm
export function px(value: number): string {
  return `${value}px`;
}

export function calculateScale(
  contentWidth: number,
  contentHeight: number,
  containerWidth: number,
  containerHeight: number
): number {
  const scaleX = containerWidth / contentWidth;
  const scaleY = containerHeight / contentHeight;
  return Math.min(scaleX, scaleY);
}

export function transformScale(scale: number): string {
  return `scale(${scale})`;
}
```

## Migration Process

### Step 1: Core Setup
1. Create project structure
2. Set up TypeScript configuration
3. Initialize core stores
4. Create basic App.svelte

### Step 2: Layout Migration
1. Implement MainLayout.svelte
2. Create responsive layout components
3. Test layout switching between screen sizes

### Step 3: Component-by-Component Migration
1. Start with leaf components (no dependencies)
2. Work up the hierarchy
3. Test each component in isolation
4. Integrate with parent components

### Step 4: State Management Integration
1. Connect components to stores
2. Implement event handling
3. Test data flow

### Step 5: Feature Implementation
1. Drag and drop
2. Undo/redo
3. Symbol selection and manipulation
4. Keyboard integration

This guide provides a concrete starting point for migrating from the Elm architecture to Svelte while maintaining the same functionality and user experience.
