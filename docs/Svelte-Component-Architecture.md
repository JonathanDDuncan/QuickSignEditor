# Svelte Component Architecture - Migration from Elm

## Svelte Component Hierarchy (Corresponding to Elm Structure)

Based on the Elm component hierarchy, here's the proposed Svelte component structure:

```
src/
├── App.svelte (Entry Point - replaces QuickSignEditor.elm + View.elm)
├── lib/
│   ├── stores/                 # Svelte stores (replaces Elm State modules)
│   │   ├── appState.js        # Global app state (replaces State.elm)
│   │   ├── layoutState.js     # Layout state (replaces Layout.State.elm)
│   │   ├── editorState.js     # Editor state (replaces SWEditor.State.elm)
│   │   ├── chooserState.js    # Chooser state (replaces Choosers.State.elm)
│   │   ├── keyboardState.js   # Keyboard state (replaces Keyboard.State.elm)
│   │   └── windowState.js     # Window size state (replaces WindowSize.State.elm)
│   ├── types/                 # TypeScript types (replaces Elm Types modules)
│   │   ├── app.ts            # Global types (replaces Types.elm)
│   │   ├── layout.ts         # Layout types (replaces Layout.State.elm types)
│   │   ├── editor.ts         # Editor types (replaces SWEditor.Types.elm)
│   │   ├── chooser.ts        # Chooser types (replaces Choosers.Types.elm)
│   │   ├── keyboard.ts       # Keyboard types (replaces Keyboard.Types.elm)
│   │   └── signwriting.ts    # SW types (replaces SW.Types.elm)
│   ├── utils/                # Utility functions (replaces Helper modules)
│   │   ├── viewHelpers.ts    # View utilities (replaces Helpers.ViewExtra.elm)
│   │   ├── maybeHelpers.ts   # Maybe utilities (replaces Helpers.MaybeExtra.elm)
│   │   ├── resultHelpers.ts  # Result utilities (replaces Helpers.ResultExtra.elm)
│   │   └── debugHelpers.ts   # Debug utilities (replaces Helpers.DebugExtra.elm)
│   ├── signwriting/          # SignWriting core library (replaces SW modules)
│   │   ├── display.ts        # Display logic (replaces SW.Display.elm)
│   │   ├── fsw.ts           # FSW handling (replaces SW.FSW.elm)
│   │   ├── sign.ts          # Sign structures (replaces SW.Sign.elm)
│   │   ├── symbol.ts        # Symbol structures (replaces SW.Symbol.elm)
│   │   ├── rectangle.ts     # Geometry (replaces SW.Rectangle.elm)
│   │   ├── pua.ts           # Unicode PUA (replaces SW.Pua.elm)
│   │   ├── identifier.ts    # Symbol ID (replaces SW.Identifier.elm)
│   │   └── portableSign.ts  # Serialization (replaces SW.PortableSign.elm)
│   └── components/          # Reusable Svelte components
│       ├── layout/          # Layout components (replaces Layout modules)
│       │   ├── MainLayout.svelte      # Main layout (replaces Layout.View.elm)
│       │   ├── AboutDialog.svelte     # About dialog (replaces Layout.AboutDialog.elm)
│       │   ├── CenterSpace.svelte     # Center area (replaces Layout.CenterSpace.elm)
│       │   ├── LeftSpace.svelte       # Left sidebar (replaces Layout.LeftSpace.elm)
│       │   ├── RightSpace.svelte      # Right sidebar (replaces Layout.RightSpace.elm)
│       │   ├── Footer.svelte          # Footer (replaces Layout.Footer.elm)
│       │   └── Drawer.svelte          # Mobile drawer (replaces Layout.Drawer.elm)
│       ├── editor/          # Sign editor components (replaces SWEditor modules)
│       │   ├── SignEditor.svelte      # Main editor (replaces SWEditor.View.elm)
│       │   ├── SignCanvas.svelte      # Sign canvas (replaces signView)
│       │   ├── EditorToolbar.svelte   # Toolbar (replaces commandsview)
│       │   ├── EditorIcons.svelte     # Icons (replaces SWEditor.Icons.elm)
│       │   ├── DragHandler.svelte     # Drag/drop (replaces SWEditor.Drag.elm)
│       │   ├── SelectionTool.svelte   # Selection (replaces SWEditor.Select.elm)
│       │   ├── RectangleSelect.svelte # Rectangle select (replaces SWEditor.RectangleSelect.elm)
│       │   └── UndoRedo.svelte        # Undo/redo (replaces SWEditor.Undo.elm)
│       ├── choosers/        # Symbol chooser components (replaces Choosers modules)
│       │   ├── ChooserContainer.svelte    # Main chooser (replaces Choosers.View.elm)
│       │   ├── Mannequin.svelte           # Body diagram (replaces Choosers.Maniquin.elm)
│       │   ├── HandGroupChooser.svelte    # Hand groups (replaces Choosers.HandGroupChooser.elm)
│       │   ├── GeneralGroupChooser.svelte # General groups (replaces Choosers.GeneralGroupChooser.elm)
│       │   ├── HandSymbolChooser.svelte   # Hand symbols (replaces Choosers.HandSymbolChooser.elm)
│       │   ├── GeneralSymbolChooser.svelte # General symbols (replaces Choosers.GeneralSymbolChooser.elm)
│       │   ├── CompassRose.svelte         # Directions (replaces Choosers.CompassRose.elm)
│       │   ├── SymbolTooltip.svelte       # Tooltips (replaces Choosers.SymbolToolTip.elm)
│       │   └── LoadingSpinner.svelte      # Loading (replaces Choosers.Loading.elm)
│       ├── keyboard/        # Virtual keyboard components (replaces Keyboard modules)
│       │   ├── VirtualKeyboard.svelte     # Main keyboard (replaces Keyboard.View.elm)
│       │   ├── KeyboardLayouts.svelte     # Layouts (replaces Keyboard.KeyboardLayouts.elm)
│       │   └── KeyboardKey.svelte         # Individual keys (replaces Keyboard.Keyboard.elm)
│       └── common/          # Common/shared components
│           ├── WindowSizeDisplay.svelte   # Window info (replaces WindowSize.View.elm)
│           ├── Button.svelte              # Reusable button
│           ├── Modal.svelte               # Modal dialog
│           └── Tooltip.svelte             # Generic tooltip
└── routes/                  # SvelteKit routes (if using routing)
    └── +page.svelte        # Main page route
```

## Component Mapping Details

### 1. Application Root
| Elm | Svelte | Notes |
|-----|--------|-------|
| QuickSignEditor.elm | App.svelte | Main application entry point |
| View.elm | App.svelte | Root view integrated into App.svelte |
| Overlay.State.elm | App.svelte + overlayState.js | Overlay logic moved to store |

### 2. Layout System
| Elm | Svelte | Notes |
|-----|--------|-------|
| Layout.View.elm | MainLayout.svelte | Responsive layout logic |
| Layout.State.elm | layoutState.js | Layout state as Svelte store |
| Layout.AboutDialog.elm | AboutDialog.svelte | Modal dialog component |
| Layout.CenterSpace.elm | CenterSpace.svelte | Center content area |
| Layout.LeftSpace.elm | LeftSpace.svelte | Left sidebar component |
| Layout.RightSpace.elm | RightSpace.svelte | Right sidebar component |
| Layout.Footer.elm | Footer.svelte | Footer/toolbar component |
| Layout.Drawer.elm | Drawer.svelte | Mobile drawer component |

### 3. Sign Editor
| Elm | Svelte | Notes |
|-----|--------|-------|
| SWEditor.View.elm | SignEditor.svelte | Main editor container |
| SWEditor.State.elm | editorState.js | Editor state as Svelte store |
| SWEditor.Types.elm | editor.ts | TypeScript type definitions |
| SWEditor.Icons.elm | EditorIcons.svelte | Icon components |
| SWEditor.Drag.elm | DragHandler.svelte | Drag/drop functionality |
| SWEditor.Select.elm | SelectionTool.svelte | Selection logic |
| SWEditor.RectangleSelect.elm | RectangleSelect.svelte | Rectangle selection |
| SWEditor.Undo.elm | UndoRedo.svelte | Undo/redo component |

### 4. Symbol Choosers
| Elm | Svelte | Notes |
|-----|--------|-------|
| Choosers.View.elm | ChooserContainer.svelte | Main chooser layout |
| Choosers.State.elm | chooserState.js | Chooser state as Svelte store |
| Choosers.Types.elm | chooser.ts | TypeScript type definitions |
| Choosers.Maniquin.elm | Mannequin.svelte | Body diagram component |
| Choosers.HandGroupChooser.elm | HandGroupChooser.svelte | Hand group selection |
| Choosers.GeneralGroupChooser.elm | GeneralGroupChooser.svelte | General group selection |
| Choosers.HandSymbolChooser.elm | HandSymbolChooser.svelte | Hand symbol selection |
| Choosers.GeneralSymbolChooser.elm | GeneralSymbolChooser.svelte | General symbol selection |

### 5. Keyboard System
| Elm | Svelte | Notes |
|-----|--------|-------|
| Keyboard.View.elm | VirtualKeyboard.svelte | Virtual keyboard display |
| Keyboard.State.elm | keyboardState.js | Keyboard state as Svelte store |
| Keyboard.Types.elm | keyboard.ts | TypeScript type definitions |
| Keyboard.KeyboardLayouts.elm | KeyboardLayouts.svelte | Layout definitions |

### 6. SignWriting Core
| Elm | Svelte | Notes |
|-----|--------|-------|
| SW.Display.elm | signwriting/display.ts | Display logic as TypeScript |
| SW.FSW.elm | signwriting/fsw.ts | FSW format handling |
| SW.Sign.elm | signwriting/sign.ts | Sign data structures |
| SW.Symbol.elm | signwriting/symbol.ts | Symbol data structures |
| All SW.*.elm | signwriting/*.ts | Core library as TypeScript modules |

## Migration Strategy

### Phase 1: Setup and Core Structure
1. Create Svelte project structure
2. Set up TypeScript configuration
3. Create basic store architecture
4. Implement core SignWriting utilities

### Phase 2: Layout System
1. Implement MainLayout.svelte with responsive design
2. Create layout component hierarchy
3. Implement layout state management
4. Test responsive behavior

### Phase 3: Sign Editor
1. Implement basic SignEditor.svelte
2. Add canvas rendering
3. Implement basic symbol manipulation
4. Add toolbar functionality

### Phase 4: Symbol Choosers
1. Implement ChooserContainer.svelte
2. Create Mannequin.svelte component
3. Add symbol group choosers
4. Implement symbol selection logic

### Phase 5: Keyboard and Advanced Features
1. Implement VirtualKeyboard.svelte
2. Add keyboard layouts
3. Implement undo/redo functionality
4. Add drag and drop

### Phase 6: Integration and Polish
1. Complete JavaScript interop
2. Add missing features
3. Performance optimization
4. Testing and debugging

## Key Differences from Elm

### State Management
- **Elm**: Centralized state with message passing
- **Svelte**: Multiple stores with reactive subscriptions

### Component Communication
- **Elm**: Messages bubble up, data flows down
- **Svelte**: Props down, events up + store subscriptions

### Type Safety
- **Elm**: Built-in type safety
- **Svelte**: TypeScript for type safety

### Reactivity
- **Elm**: Virtual DOM with immutable updates
- **Svelte**: Compiled reactivity with mutable updates

This architecture maintains the same functionality as the Elm version while leveraging Svelte's strengths in component composition and reactivity.
