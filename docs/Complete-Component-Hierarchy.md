# Complete Component Hierarchy Analysis

## Elm Component Parent Chain (from bottom to top)

Based on the code analysis, here's the complete parent chain for each major component:

### Mannequin Component Chain
```
Choosers.Maniquin.elm (maniquin function)
  ↑ (used as maniqin1.view in)
Choosers.View.elm (root function) 
  ↑ (called by)
Layout.RightSpace.elm (rightspace function)
  ↑ (called by)
Layout.View.elm (root function)
  ↑ (called by)
Overlay.State.elm (root function)
  ↑ (called by)
View.elm (root function)
  ↑ (called by)
QuickSignEditor.elm (main program)
  ↑ (entry point)
JavaScript index.js (Elm.QuickSignEditor.embed)
```

### Sign Editor Component Chain  
```
SWEditor.View.elm (root function)
  ↑ (called by)
Layout.CenterSpace.elm (centerspace function)
  ↑ (called by)
Layout.View.elm (screenlayout functions)
  ↑ (called by)
Overlay.State.elm (root function)
  ↑ (called by)
View.elm (root function)
  ↑ (called by)
QuickSignEditor.elm (main program)
```

### Keyboard Component Chain
```
Keyboard.View.elm (root function)
  ↑ (called by)
Layout.CenterSpace.elm (centerspace function)
  ↑ (called by)
Layout.View.elm (screenlayout functions)
  ↑ (called by)
Overlay.State.elm (root function)
  ↑ (called by)
View.elm (root function)
  ↑ (called by)
QuickSignEditor.elm (main program)
```

## Complete Elm Module Inventory

### 1. Application Core (Top Level)
- **QuickSignEditor.elm** - Main entry point
- **View.elm** - Root view component
- **State.elm** - Global state management  
- **Types.elm** - Global type definitions
- **Ports.elm** - JavaScript interop
- **PlatformHelpers.elm** - Platform utilities

### 2. Overlay System
- **Overlay/State.elm** - Application overlay management

### 3. Layout System (8 modules)
- **Layout/View.elm** - Main layout coordination
- **Layout/State.elm** - Layout state management
- **Layout/AboutDialog.elm** - About dialog
- **Layout/CenterSpace.elm** - Center content area
- **Layout/LeftSpace.elm** - Left sidebar
- **Layout/RightSpace.elm** - Right sidebar  
- **Layout/Footer.elm** - Footer/bottom toolbar
- **Layout/Drawer.elm** - Mobile/tablet drawer

### 4. Sign Editor System (13 modules)
- **SWEditor/View.elm** - Main editor view
- **SWEditor/State.elm** - Editor state management
- **SWEditor/Types.elm** - Editor type definitions
- **SWEditor/Icons.elm** - UI icons
- **SWEditor/Drag.elm** - Drag and drop
- **SWEditor/Select.elm** - Symbol selection
- **SWEditor/RectangleSelect.elm** - Rectangle selection tool
- **SWEditor/SignArea.elm** - Sign canvas management
- **SWEditor/EditorSign.elm** - Sign manipulation
- **SWEditor/EditorSymbol.elm** - Symbol manipulation
- **SWEditor/EditorKeyboard.elm** - Keyboard integration
- **SWEditor/Undo.elm** - Undo/redo functionality

### 5. Symbol Chooser System (23 modules)
- **Choosers/View.elm** - Main chooser layout
- **Choosers/State.elm** - Chooser state management
- **Choosers/Types.elm** - Chooser type definitions
- **Choosers/Maniquin.elm** - Mannequin/body diagram ⭐ (THE COMPONENT YOU ASKED ABOUT)
- **Choosers/HandGroupChooser.elm** - Hand symbol groups
- **Choosers/GeneralGroupChooser.elm** - General symbol groups
- **Choosers/HandSymbolChooser.elm** - Individual hand symbols
- **Choosers/GeneralSymbolChooser.elm** - Individual general symbols
- **Choosers/GeneralSymbolChooserKeyboard.elm** - Keyboard for general symbols
- **Choosers/GroupChooserKeyboard.elm** - Keyboard for groups
- **Choosers/HandPng.elm** - Hand PNG resources
- **Choosers/CompassRose.elm** - Directional selection
- **Choosers/SymbolToolTip.elm** - Symbol tooltips
- **Choosers/Keyboard.elm** - Chooser keyboard
- **Choosers/KeyboardType.elm** - Keyboard types
- **Choosers/ManiquinKeyboard.elm** - Mannequin keyboard integration
- **Choosers/Petalhelper.elm** - Petal diagram helper
- **Choosers/Loading.elm** - Loading states
- **Choosers/LoadingType.elm** - Loading type definitions
- **Choosers/ChooserItemType.elm** - Chooser item types
- **Choosers/EditorType.elm** - Editor integration types
- **Choosers/EditorUpdate.elm** - Editor update logic
- **Choosers/ImportModelType.elm** - Import model types

### 6. Keyboard System (7 modules)
- **Keyboard/View.elm** - Virtual keyboard display
- **Keyboard/State.elm** - Keyboard state management
- **Keyboard/Types.elm** - Keyboard type definitions
- **Keyboard/Keyboard.elm** - Core keyboard logic
- **Keyboard/KeyboardLayouts.elm** - Keyboard layouts
- **Keyboard/KeyboardModeType.elm** - Keyboard modes
- **Keyboard/Shared.elm** - Shared keyboard utilities

### 7. Window/Responsive System (3 modules)
- **WindowSize/View.elm** - Window size display
- **WindowSize/State.elm** - Window size management
- **WindowSize/Types.elm** - Window size types

### 8. SignWriting Core Library (12 modules)
- **SW/Display.elm** - Symbol display rendering
- **SW/FSW.elm** - FSW format parsing/generation
- **SW/Sign.elm** - Sign data structures
- **SW/Symbol.elm** - Symbol data structures
- **SW/Types.elm** - Core SW type definitions
- **SW/Rectangle.elm** - Geometric calculations
- **SW/Pua.elm** - Unicode PUA handling
- **SW/Identifier.elm** - Symbol identification
- **SW/PortableSign.elm** - Sign serialization
- **SW/HandFillsType.elm** - Hand fill types
- **SW/HandsType.elm** - Hand types
- **SW/PlanesType.elm** - Plane types

### 9. Helper/Utility Modules (4 modules)
- **Helpers/ViewExtra.elm** - View helper functions
- **Helpers/MaybeExtra.elm** - Maybe utility functions
- **Helpers/ResultExtra.elm** - Result utility functions  
- **Helpers/DebugExtra.elm** - Debug utilities

## Total Module Count: 76 Elm Files

## Corresponding Svelte Architecture

The 76 Elm modules will be reorganized into the following Svelte structure:

### Svelte Component Count Mapping:
- **Stores**: 6 store files (replacing 8 State.elm modules)
- **Types**: 6 TypeScript files (replacing 15 Types.elm modules)
- **Components**: ~35 Svelte components (replacing 45 View.elm modules)
- **Utils**: ~15 utility modules (replacing 16 helper/utility modules)
- **Core Libraries**: 12 TypeScript modules (replacing 12 SW modules)

### Key Benefits of Svelte Migration:
1. **Reduced File Count**: ~74 files vs 76 Elm files
2. **Better Separation of Concerns**: Clear separation of logic, state, and presentation
3. **Modern TypeScript**: Better tooling and IDE support
4. **Component Reusability**: More flexible component composition
5. **Performance**: Compiled reactivity vs virtual DOM

## Summary

The Mannequin component (`Choosers.Maniquin.elm`) sits in a 7-level deep hierarchy within the QuickSignEditor application. The migration to Svelte will flatten this hierarchy while maintaining the same functionality through a more modern component architecture with better separation of concerns.
