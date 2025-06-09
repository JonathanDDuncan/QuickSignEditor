# Complete Svelte Component Tree Structure

This document shows the actual component hierarchy and nesting relationships in the Svelte implementation of QuickSignEditor, similar to the original Elm component tree structure.

## Actual Component Tree (Current Implementation)

### Current Flattened Structure in App.svelte
```
App.svelte
├── Toolbar.svelte
├── SymbolChooser.svelte (in editor-layout)
├── SignEditor.svelte (in editor-layout)
├── ChooserPanel.svelte
│   ├── SymbolChooser.svelte (nested, reused)
│   ├── HandGroupChooser.svelte
│   ├── GeneralGroupChooser.svelte
│   └── Maniquin.svelte (nested)
└── Maniquin.svelte (standalone)
```

**Template Structure:**
```svelte
<main>
  <Toolbar />
  <div class="editor-layout">
    <SymbolChooser />
    <SignEditor />
  </div>
  <ChooserPanel />
  <Maniquin />
</main>
```

### Corrected Component Count
- **Actual Components**: 8 unique components (not 10)
- **Component Reuse**: SymbolChooser and Maniquin used twice
- **Layout**: Everything directly in App.svelte, no proper layout separation

### Recommended Layout Structure (Matching Elm Architecture)

To properly match the Elm implementation, the structure should be:

```
App.svelte
├── MainLayout.svelte
│   ├── AboutDialog.svelte
│   ├── ScreenLayout.svelte
│   │   ├── LeftSpace.svelte
│   │   ├── CenterSpace.svelte
│   │   │   ├── SignEditor.svelte
│   │   │   │   ├── Toolbar.svelte (commandsview)
│   │   │   │   ├── SignCanvas.svelte (signView)
│   │   │   │   └── RectangleSelect.svelte
│   │   │   └── VirtualKeyboard.svelte
│   │   ├── RightSpace.svelte
│   │   │   └── ChooserPanel.svelte
│   │   │       ├── Maniquin.svelte
│   │   │       ├── SymbolChooser.svelte
│   │   │       ├── HandGroupChooser.svelte
│   │   │       └── GeneralGroupChooser.svelte
│   │   └── Drawer.svelte (mobile/tablet)
│   └── Footer.svelte
└── SymbolSVG.svelte (utility component)
```

### Missing Layout Components from Elm

The Elm implementation has a sophisticated layout system that the current Svelte version lacks:

#### **Layout.View.elm Components:**
1. **MainLayout** - Overall responsive container
2. **ScreenLayout** - Main content grid (left/center/right)
3. **LeftSpace** - Left sidebar (currently unused but available)
4. **CenterSpace** - Main editor area
5. **RightSpace** - Chooser panel area
6. **AboutDialog** - Modal overlay
7. **Footer** - Sticky bottom bar
8. **Drawer** - Mobile/tablet navigation

#### **Current Problem:**
The current implementation puts everything directly in `App.svelte`, which means:
- No responsive layout switching
- No proper sidebar management  
- SignEditor and Choosers are siblings instead of being in separate layout areas
- Missing mobile/tablet experience
- No modal system for dialogs

#### **Layout Comparison:**

**Elm Layout (Proper):**
```
Layout.View.screenlayout
├── Layout.LeftSpace (sidebar)
├── Layout.CenterSpace (editor)
│   └── SWEditor.View.root
└── Layout.RightSpace (choosers)
    └── Choosers.View.root
```

**Current Svelte (Flattened):**
```
App.svelte
├── Toolbar (should be in CenterSpace)
├── editor-layout div
│   ├── SymbolChooser (should be in RightSpace)
│   └── SignEditor (should be in CenterSpace)
└── ChooserPanel (should be in RightSpace)
```

## Component Details with Rendering Context

### App.svelte (Entry Point)
**Renders directly:**
- `<SignEditor>` - Main editing canvas
- `<SymbolChooser>` - Top-level symbol chooser
- `<Toolbar>` - Editor actions (undo/redo/etc.)
- `<ChooserPanel>` - Complex chooser interface
- `<Maniquin>` - Standalone preview section

**Template structure:**
```svelte
<main>
  <Toolbar />
  <div class="editor-layout">
    <SymbolChooser />
    <SignEditor />
  </div>
  <ChooserPanel />
  <Maniquin />
</main>
```

### ChooserPanel.svelte (Container Panel)
**Renders directly:**
- `<SymbolChooser>` - Symbol selection interface
- `<HandGroupChooser>` - Hand symbol groups
- `<GeneralGroupChooser>` - General symbol groups  
- `<Maniquin>` - Hand preview area

**Template structure:**
```svelte
<div class="chooser-panel">
  <div class="maniquin-area">
    <Maniquin />
  </div>
  <div class="main-chooser">
    <SymbolChooser />
  </div>
  <div class="subgroup-chooser">
    <!-- Conditional rendering -->
    {#if isHand}
      <HandGroupChooser />
    {:else}
      <GeneralGroupChooser />
    {/if}
  </div>
</div>
```

### Leaf Components (No Child Components)

#### SignEditor.svelte
- **Purpose:** Main editing canvas for sign symbols
- **Renders:** Symbol display elements directly in template
- **Dependencies:** Uses `SymbolSVG` for individual symbol rendering

#### SymbolChooser.svelte
- **Purpose:** Symbol selection interface
- **Renders:** Symbol list, search, category filters
- **Dependencies:** May use `SymbolSVG` for symbol display

#### Toolbar.svelte
- **Purpose:** Editor action buttons
- **Renders:** Button elements directly

#### Maniquin.svelte
- **Purpose:** Hand preview/mannequin display
- **Renders:** SVG content for hand representation
- **Dependencies:** Uses SignWriting font utilities

#### SymbolSVG.svelte
- **Purpose:** Individual symbol rendering
- **Renders:** SVG elements for SignWriting symbols
- **Dependencies:** SignWriting font generation utilities

#### HandGroupChooser.svelte
- **Purpose:** Hand-specific symbol groups (stub)
- **Renders:** List of hand-related symbols

#### GeneralGroupChooser.svelte
- **Purpose:** General symbol groups (stub)
- **Renders:** List of general symbols

## Component Reuse Patterns

### SymbolChooser.svelte
- **Used in:** `App.svelte` (top-level) and `ChooserPanel.svelte` (nested)
- **Different contexts:** Standalone vs. panel-embedded

### Maniquin.svelte  
- **Used in:** `App.svelte` (standalone) and `ChooserPanel.svelte` (embedded)
- **Different contexts:** Debug preview vs. integrated chooser preview

### SymbolSVG.svelte
- **Used in:** Multiple components for symbol rendering
- **Purpose:** Shared symbol display utility

## Architecture Comparison: Elm vs Svelte

### Elm Original (76 files, 7 levels deep)
```
Main.elm
├── View.elm
│   ├── ChooserPanel.elm
│   │   ├── SymbolChooser.elm
│   │   │   ├── SymbolList.elm
│   │   │   │   ├── SymbolRow.elm
│   │   │   │   │   └── SymbolView.elm
│   │   │   └── CategoryFilter.elm
│   │   ├── HandGroupChooser.elm
│   │   └── GeneralGroupChooser.elm
│   ├── SignEditor.elm
│   ├── Toolbar.elm
│   └── Mannequin.elm
└── [Many more nested modules...]
```

### Svelte Implementation (8 unique components, flattened structure)
```
App.svelte
├── ChooserPanel.svelte
│   ├── SymbolChooser.svelte (reused)
│   ├── HandGroupChooser.svelte
│   ├── GeneralGroupChooser.svelte
│   └── Maniquin.svelte (reused)
├── SignEditor.svelte
├── Toolbar.svelte
├── SymbolChooser.svelte (top-level)
├── Maniquin.svelte (standalone)
└── SymbolSVG.svelte
```

**Note:** This simplified structure is missing the proper layout components from the Elm implementation. SymbolChooser and Maniquin are reused in multiple contexts.

### Proper Svelte Structure (Matching Elm Layout)
```
App.svelte
├── MainLayout.svelte
│   ├── AboutDialog.svelte
│   ├── ScreenLayout.svelte
│   │   ├── LeftSpace.svelte
│   │   ├── CenterSpace.svelte
│   │   │   ├── SignEditor.svelte
│   │   │   │   ├── Toolbar.svelte (commandsview)
│   │   │   │   ├── SignCanvas.svelte (signView)
│   │   │   │   └── RectangleSelect.svelte
│   │   │   └── VirtualKeyboard.svelte
│   │   ├── RightSpace.svelte
│   │   │   └── ChooserPanel.svelte
│   │   │       ├── Maniquin.svelte
│   │   │       ├── SymbolChooser.svelte
│   │   │       ├── HandGroupChooser.svelte
│   │   │       └── GeneralGroupChooser.svelte
│   │   └── Drawer.svelte (mobile/tablet)
│   └── Footer.svelte
└── SymbolSVG.svelte (utility component)
```

### Missing Layout Components Analysis

The current implementation flattens everything into `App.svelte`, but the Elm structure shows clear layout separation:

#### **Missing Components:**
1. **MainLayout.svelte** - Overall application layout container
2. **LeftSpace.svelte** - Left sidebar area (currently empty in Elm)
3. **CenterSpace.svelte** - Main content area containing the editor
4. **RightSpace.svelte** - Right sidebar containing choosers
5. **AboutDialog.svelte** - Modal dialog component
6. **Footer.svelte** - Bottom footer area
7. **Drawer.svelte** - Mobile/tablet drawer navigation
8. **VirtualKeyboard.svelte** - On-screen keyboard component

#### **What Happened to These Components:**
In the current Svelte implementation, these layout responsibilities were absorbed into:
- **App.svelte** - Contains direct references to all components
- **ChooserPanel.svelte** - Partially replaces RightSpace functionality
- No responsive layout switching between desktop/mobile modes
- No proper sidebar management

## Key Architectural Differences

### 1. Flattened Hierarchy
- **Elm:** Deep nesting (7 levels) with many intermediate modules
- **Svelte:** Shallow nesting (2-3 levels) with direct component composition

### 2. Component Reuse
- **Elm:** Each component instance had separate modules
- **Svelte:** Same component used in multiple contexts with different props

### 3. State Management
- **Elm:** Message passing through deep component tree
- **Svelte:** Centralized stores (`signStore`, `signModel`) + event dispatching

### 4. Template vs Logic Separation
- **Elm:** View functions mixed with model updates
- **Svelte:** Clear separation of script, template, and styles

## Implementation Status

### ✅ Fully Implemented (6 components)
- `App.svelte` - Complete application shell
- `SignEditor.svelte` - Full editing functionality
- `SymbolChooser.svelte` - Working symbol selection
- `Toolbar.svelte` - All toolbar actions
- `Maniquin.svelte` - Hand preview with FSW rendering
- `SymbolSVG.svelte` - Symbol display utility

### 🚧 Partially Implemented (1 component)
- `ChooserPanel.svelte` - Layout complete, needs integration refinement

### 📝 Stub Implementation (2 components)
- `HandGroupChooser.svelte` - Placeholder structure
- `GeneralGroupChooser.svelte` - Placeholder structure

## Data Flow Patterns

### Props Down, Events Up
```svelte
<!-- Parent to Child: Props -->
<SymbolChooser {symbols} {filter} {categories} />

<!-- Child to Parent: Events -->
<SymbolChooser on:select={handleSymbolSelect} />
```

### Store-Based State Sharing
```javascript
// Shared across components
import { signStore, signModel } from './signStore';
$: signData = $signStore.present;
```

### Component Communication Examples

#### App.svelte → SignEditor.svelte
```svelte
<SignEditor {signData} on:select={handleSymbolSelection} />
```

#### App.svelte → ChooserPanel.svelte
```svelte
<ChooserPanel model={chooserModel} parentWidth={800} parentHeight={400} />
```

#### ChooserPanel.svelte → Maniquin.svelte
```svelte
<Maniquin {model} />
```

This simplified architecture maintains the functionality of the original Elm implementation while reducing complexity and improving maintainability through Svelte's reactive patterns and component reuse capabilities.
