# Svelte Component Hierarchy - Actual Implementation

## Overview
This document describes the **actual implemented** Svelte component structure in the QuickSignEditor project, as opposed to the theoretical architecture originally planned. The implementation is simpler and more streamlined than the original Elm-to-Svelte mapping.

## Actual Project Structure

```
QuickSignEditor/
├── src/
│   ├── App.svelte                    # Main application entry point
│   ├── lib/                          # Core Svelte library components
│   │   ├── ChooserPanel.svelte       # Main symbol chooser panel
│   │   ├── GeneralGroupChooser.svelte # General group selection component
│   │   ├── HandGroupChooser.svelte   # Hand group selection component
│   │   ├── Maniquin.svelte           # Mannequin body diagram (target component)
│   │   ├── SignEditor.svelte         # Main sign editor canvas
│   │   ├── SymbolChooser.svelte      # Symbol selection interface
│   │   ├── SymbolSVG.svelte          # SVG symbol rendering component
│   │   ├── Toolbar.svelte            # Editor toolbar
│   │   ├── signStore.ts              # Main application state store
│   │   ├── signWritingFont.ts        # SignWriting font utilities
│   │   └── utils/                    # Utility functions
│   │       ├── clipboard.ts          # Clipboard operations
│   │       ├── fsw.ts                # FSW format handling
│   │       ├── history.ts            # Undo/redo functionality
│   │       ├── transform.ts          # Symbol transformations
│   │       ├── types.ts              # TypeScript type definitions
│   │       └── validation.ts         # Data validation
│   ├── legacy/                       # Legacy data files
│   │   └── chooserclassification.json # Symbol classification data
│   └── docs/                         # Documentation files
└── tests/                            # Test files
```

## Component Hierarchy

### App.svelte (Root Component)
**Purpose**: Main application entry point and layout container
**Dependencies**: All other components

```svelte
App.svelte
├── SignEditor.svelte
├── SymbolChooser.svelte
├── Toolbar.svelte
├── ChooserPanel.svelte
└── Maniquin.svelte
```

### Core Components

#### 1. SignEditor.svelte
- **Purpose**: Main canvas for editing SignWriting symbols
- **Features**: 
  - Symbol positioning and selection
  - Drag and drop functionality
  - Canvas interaction handling
  - Undo/redo integration
- **Dependencies**: `signStore`, `validation`, `transform`, `types`

#### 2. ChooserPanel.svelte
- **Purpose**: Main symbol selection interface
- **Features**:
  - Symbol browsing and filtering
  - Category-based organization
  - Sub-component coordination
- **Dependencies**: 
  - `SymbolChooser.svelte`
  - `HandGroupChooser.svelte`
  - `GeneralGroupChooser.svelte`
  - `Maniquin.svelte`

#### 3. Maniquin.svelte (Target Component)
- **Purpose**: Body diagram for symbol placement (equivalent to Elm's `Choosers.Maniquin.elm`)
- **Features**:
  - Displays SignWriting symbols on body diagram
  - Handles complete FSW rendering
  - Supports individual symbol positioning
  - Interactive symbol selection
- **Dependencies**: `signWritingFont.ts`, `signStore`

#### 4. SymbolChooser.svelte
- **Purpose**: Symbol library and selection interface
- **Features**:
  - Symbol grid display
  - Search and filtering
  - Category navigation
- **Dependencies**: `signStore`

#### 5. Toolbar.svelte
- **Purpose**: Editor action controls
- **Features**:
  - Undo/redo buttons
  - Copy/paste operations
  - File operations
- **Dependencies**: `signStore`, `clipboard`

#### 6. SymbolSVG.svelte
- **Purpose**: SVG rendering for individual symbols
- **Features**:
  - Dual-font SignWriting rendering
  - Symbol scaling and positioning
- **Dependencies**: `signWritingFont.ts`

### Support Components

#### 7. HandGroupChooser.svelte
- **Purpose**: Hand-specific symbol group selection
- **Features**: Category-specific symbol browsing
- **Status**: Stub implementation

#### 8. GeneralGroupChooser.svelte
- **Purpose**: General symbol group selection
- **Features**: General category symbol browsing
- **Status**: Stub implementation

## State Management

### Primary Store: signStore.ts
- **Purpose**: Central application state management
- **Features**:
  - Sign data management
  - History tracking (undo/redo)
  - Symbol manipulation
  - Data persistence
- **Structure**:
  ```typescript
  interface SignData {
    name: string;
    symbols: Symbol[];
    metadata: object;
  }
  ```

### Additional State: signModel
- **Purpose**: Mannequin-specific data
- **Features**:
  - Symbol positioning for body diagram
  - FSW rendering data
  - Legacy data integration

## Key Differences from Original Elm Structure

### Simplified Architecture
- **Elm**: 76 files across 9 major systems
- **Svelte**: ~10 main components with focused responsibilities

### Flattened Hierarchy
- **Elm**: 7-level deep component nesting
- **Svelte**: 2-3 level maximum depth

### Consolidated State Management
- **Elm**: Multiple state modules with message passing
- **Svelte**: Centralized stores with reactive subscriptions

### Integrated Utilities
- **Elm**: Separate modules for each utility
- **Svelte**: Combined utilities in `utils/` directory

## Mannequin Component Implementation

The `Maniquin.svelte` component is the direct equivalent of the Elm `Choosers.Maniquin.elm` component:

### Features
- **Complete FSW Rendering**: Handles full SignWriting signs
- **Individual Symbol Positioning**: Places symbols on body diagram
- **Interactive Selection**: Click and drag symbol interaction
- **Dual-Font Rendering**: Uses SuttonSignWriting fonts
- **Responsive Scaling**: Adapts to different container sizes

### Integration
- **Parent**: `ChooserPanel.svelte` and `App.svelte`
- **Data Source**: `signStore.signModel`
- **Rendering**: `signWritingFont.ts` for SVG generation

## Testing Structure

Each component has corresponding test files:
- `ChooserPanel.test.ts`
- `SignEditor.test.ts`
- `SymbolChooser.test.ts`
- `Toolbar.test.ts`
- Utility tests in `utils/` directory

## Migration Status

### Completed
- ✅ Core component structure
- ✅ State management with stores
- ✅ Mannequin component functionality
- ✅ Dual-font SignWriting rendering
- ✅ Basic symbol manipulation
- ✅ Undo/redo functionality

### In Progress
- 🔄 Advanced symbol chooser features
- 🔄 Complete FSW import/export
- 🔄 Advanced editing operations

### Planned
- 📋 Virtual keyboard integration
- 📋 Advanced layout responsiveness
- 📋 Complete Elm feature parity

## Conclusion

The actual Svelte implementation is significantly simpler and more maintainable than the original Elm structure while maintaining the core functionality. The `Maniquin.svelte` component successfully replicates the behavior of the Elm `Choosers.Maniquin.elm` component with modern Svelte patterns and improved performance.

The architecture demonstrates a successful migration from Elm's complex hierarchical structure to Svelte's more straightforward component-based approach, with particular success in the mannequin/body diagram functionality that was the focus of the original analysis.
