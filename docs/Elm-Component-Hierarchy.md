# Elm Component Hierarchy Documentation

## Complete Component Tree Structure

Based on the codebase analysis, here is the complete Elm component hierarchy from top to bottom:

```
QuickSignEditor.main (Entry Point)
└── View.root
    └── Overlay.State.root
        └── Layout.View.root
            ├── Layout.AboutDialog.aboutdialog
            ├── Layout.View.screenlayout
            │   ├── Layout.LeftSpace.leftspace
            │   ├── Layout.CenterSpace.centerspace
            │   │   ├── SWEditor.View.root
            │   │   │   ├── SWEditor.View.commandsview (Toolbar)
            │   │   │   ├── SWEditor.View.signView (Main Canvas)
            │   │   │   └── SWEditor.View.rectangleselect
            │   │   └── Keyboard.View.root
            │   ├── Layout.RightSpace.rightspace
            │   │   └── Choosers.View.root
            │   │       ├── Choosers.Maniquin.maniquin (maniqin1.view)
            │   │       ├── Choosers.View.getsymbolchooser
            │   │       │   ├── Choosers.HandSymbolChooser.handsymbolchooser
            │   │       │   └── Choosers.GeneralSymbolChooser.generalsymbolchooser
            │   │       └── Choosers.View.choosesubgroupchooser
            │   │           ├── Choosers.HandGroupChooser.handgroupchooser
            │   │           └── Choosers.GeneralGroupChooser.generalgroupchooser
            │   └── Layout.Drawer.drawer (for medium/small screens)
            └── Layout.Footer.stickyFooter
```

## Detailed Component Breakdown

### 1. Application Entry Point
- **QuickSignEditor.elm** - Main Elm program entry point
- **View.elm** - Root view component
- **Types.elm** - Global type definitions
- **State.elm** - Global state management

### 2. Overlay System
- **Overlay.State.elm** - Application overlay and integration management

### 3. Layout System (Layout/)
- **Layout.View.elm** - Main layout root with responsive design
- **Layout.State.elm** - Layout state management
- **Layout.AboutDialog.elm** - About dialog component
- **Layout.CenterSpace.elm** - Center content area
- **Layout.LeftSpace.elm** - Left sidebar area
- **Layout.RightSpace.elm** - Right sidebar area (contains Choosers)
- **Layout.Footer.elm** - Bottom toolbar/footer
- **Layout.Drawer.elm** - Collapsible drawer for mobile/tablet

### 4. Sign Editor Core (SWEditor/)
- **SWEditor.View.elm** - Main sign editor view
- **SWEditor.State.elm** - Editor state management
- **SWEditor.Types.elm** - Editor type definitions
- **SWEditor.Icons.elm** - UI icons for editor
- **SWEditor.Drag.elm** - Drag and drop functionality
- **SWEditor.Select.elm** - Symbol selection logic
- **SWEditor.RectangleSelect.elm** - Rectangle selection tool
- **SWEditor.SignArea.elm** - Sign canvas area management
- **SWEditor.EditorSign.elm** - Sign manipulation
- **SWEditor.EditorSymbol.elm** - Symbol manipulation
- **SWEditor.EditorKeyboard.elm** - Keyboard integration
- **SWEditor.Undo.elm** - Undo/redo functionality

### 5. Symbol Choosers (Choosers/)
- **Choosers.View.elm** - Main chooser layout
- **Choosers.State.elm** - Chooser state management
- **Choosers.Types.elm** - Chooser type definitions
- **Choosers.Maniquin.elm** - Mannequin/body diagram component
- **Choosers.HandGroupChooser.elm** - Hand symbol group selection
- **Choosers.GeneralGroupChooser.elm** - General symbol group selection
- **Choosers.HandSymbolChooser.elm** - Individual hand symbol selection
- **Choosers.GeneralSymbolChooser.elm** - Individual general symbol selection
- **Choosers.CompassRose.elm** - Directional selection component
- **Choosers.SymbolToolTip.elm** - Symbol tooltip display
- **Choosers.Keyboard.elm** - Chooser keyboard integration
- **Choosers.Loading.elm** - Loading state component

### 6. Keyboard System (Keyboard/)
- **Keyboard.View.elm** - Virtual keyboard display
- **Keyboard.State.elm** - Keyboard state management
- **Keyboard.Types.elm** - Keyboard type definitions
- **Keyboard.Keyboard.elm** - Core keyboard logic
- **Keyboard.KeyboardLayouts.elm** - Different keyboard layouts
- **Keyboard.KeyboardModeType.elm** - Keyboard modes
- **Keyboard.Shared.elm** - Shared keyboard utilities

### 7. Window/Responsive System (WindowSize/)
- **WindowSize.View.elm** - Window size display component
- **WindowSize.State.elm** - Window size state management
- **WindowSize.Types.elm** - Window size type definitions

### 8. SignWriting Core Library (SW/)
- **SW.Display.elm** - Symbol display logic
- **SW.FSW.elm** - FSW format handling
- **SW.Sign.elm** - Sign data structures
- **SW.Symbol.elm** - Symbol data structures
- **SW.Types.elm** - Core SW type definitions
- **SW.Rectangle.elm** - Geometric calculations
- **SW.Pua.elm** - Unicode PUA handling
- **SW.Identifier.elm** - Symbol identification
- **SW.PortableSign.elm** - Sign serialization

### 9. Utility Modules
- **Helpers.ViewExtra.elm** - View helper functions
- **Helpers.MaybeExtra.elm** - Maybe utility functions
- **Helpers.ResultExtra.elm** - Result utility functions
- **Helpers.DebugExtra.elm** - Debug utilities
- **PlatformHelpers.elm** - Platform abstraction
- **Ports.elm** - JavaScript interop

## Component Interaction Patterns

### Message Flow
1. User interactions start from View components
2. Messages flow up through the component hierarchy
3. State updates are managed at appropriate levels
4. View updates propagate down the hierarchy

### Data Flow
1. Global state is managed in `State.elm`
2. Module-specific state is managed in respective `State.elm` files
3. Props/data flow down from parent to child components
4. Events bubble up through message passing

### Responsive Design
- **Wide Screen (>1000px)**: Three-column layout (Left + Center + Right)
- **Medium Screen (600-1000px)**: Two-column layout (Center + Right) with collapsible left
- **Small Screen (<600px)**: Single-column layout with drawer overlay

## Key Integration Points

1. **Layout.RightSpace** → **Choosers.View** (Symbol selection interface)
2. **Layout.CenterSpace** → **SWEditor.View** (Main editing canvas)
3. **Layout.CenterSpace** → **Keyboard.View** (Virtual keyboard)
4. **Choosers.View** → **Choosers.Maniquin** (Body diagram for symbol placement)
5. **SWEditor.View** → Multiple editor sub-components (toolbar, canvas, selection)

This hierarchy forms the foundation for the Svelte component architecture that will replace the Elm implementation.
