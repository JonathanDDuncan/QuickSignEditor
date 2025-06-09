# QuickSignEditor 2025 - Svelte 3 Implementation Guide

## Project Overview

This implementation guide provides the technical roadmap for QuickSignEditor 2025, a modern rewrite of the legacy Elm-based SignWriting editor as a Svelte 3 application. The project emphasizes maintainability, comprehensive testing, and AI-optimized development patterns.

### Live Reference Application
**Current Elm Version**: https://jonathandduncan.github.io/QuickSignEditor/
- Use this live version to verify feature parity and behavior
- Compare selection tools, editor modes, and user interactions
- Test FSW import/export functionality against live version

### Key Project Goals
- **Feature Parity**: Full compatibility with legacy Elm application functionality
- **Modern Architecture**: Svelte 3 + TypeScript with reactive state management
- **Enhanced UX**: Improved mobile responsiveness and accessibility (WCAG 2.1 AA)
- **Comprehensive Testing**: Three-tier testing approach with 90%+ coverage
- **AI-Optimized Development**: Small functions, clear naming, pure function architecture

## Core Application Features

### Sign Editor Features (From PRD Requirements)
- **Canvas Operations**: Drag, drop, resize, rotate, and color symbols on canvas
- **Symbol Library**: Browse, search, and filter comprehensive symbol collection
- **History Management**: Full undo/redo with pure function state transitions
- **Clipboard Operations**: Copy, paste, and align symbols or groups
- **FSW Integration**: Import/export signs in Formal SignWriting format
- **Responsive Design**: Seamless desktop, tablet, and mobile experience
- **Accessibility**: Full keyboard navigation, screen reader support, high-contrast mode
- **External Integration**: Communication with SignWriter Studio and other applications

### Browser Compatibility Requirements
- Chrome, Firefox, Edge, Opera, Safari
- Major mobile browsers
- Progressive Web App capabilities

## Development Process Overview

### AI-Optimized Workflow
- **Small Functions**: ≤20 lines for better GitHub Copilot suggestions
- **Clear Names**: Intention-revealing function and variable names  
- **TypeScript**: Use TypeScript for better AI understanding and type safety
- **Pure Functions First**: Business logic without side effects
- **Component Isolation**: Test components in isolation before integration
- **Numbered Options for User Input**: When presenting the user with multiple options, always use a numbered list so the user can answer with a number.

### Testing-First Development Philosophy
- **Red-Green-Refactor**: Write failing tests first, implement minimal code, then refactor
- **Test Isolation**: Each test should be independent and atomic
- **Mock Strategically**: Mock external dependencies but not the code under test
- **Debug Incrementally**: Use console.log, debugger statements, and test-driven debugging

### Three-Tier Development Approach

#### Tier 1: Pure Functions (TypeScript, no mocks)
```typescript
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { validateSignData } from '../lib/utils/validation';

describe('validateSignData', () => {
  beforeEach(() => {
    // Reset any global state if needed
  });

  afterEach(() => {
    // Clean up after each test if needed
  });

  it('returns valid for complete data', () => {
    const input = { name: 'test', symbols: ['symbol1'] };
    const result = validateSignData(input);
    expect(result.isValid).toBe(true);
    expect(result).toMatchSnapshot();
  });

  it('returns invalid for missing name', () => {
    const input = { symbols: ['symbol1'] };
    const result = validateSignData(input);
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('Name required');
  });

  it.each([
    [null, false, 'Invalid input'],
    [undefined, false, 'Invalid input'],
    [{}, false, 'Name required'],
    [{ name: '' }, false, 'Name required'],
    [{ name: 'test' }, false, 'Symbols required'],
    [{ name: 'test', symbols: null }, false, 'Invalid symbols'],
    [{ name: 'test', symbols: [] }, true, ''],
  ])('validates input %p correctly', (input, expectedValid, expectedError) => {
    const result = validateSignData(input);
    expect(result.isValid).toBe(expectedValid);
    if (!expectedValid) {
      expect(result.error).toContain(expectedError);
    }
  });

  it('performs well with large symbol arrays', () => {
    const largeSymbolArray = Array(1000).fill('').map((_, i) => `symbol${i}`);
    const input = { name: 'test', symbols: largeSymbolArray };
    const start = performance.now();
    const result = validateSignData(input);
    const end = performance.now();
    expect(result.isValid).toBe(true);
    expect(end - start).toBeLessThan(10);
  });
});
```

#### Tier 2: Svelte Components
Build isolated, testable components:
```svelte
<!-- SignEditor.svelte -->
<script>
  import { validateSignData } from './validation.js';
  
  export let signData = {};
  
  $: validation = validateSignData(signData);
</script>

{#if !validation.isValid}
  <div class="error">{validation.error}</div>
{/if}
```

#### Tier 3: Integration & E2E
Full application workflows with real data and user interactions.

### Testing Commands
```powershell
# Run all tests
npm test

# Run with coverage
npm run test:coverage

# Run specific test file
npm test -- SignEditor.test.js

# Run tests in watch mode
npm test -- --watch

# Run tests with debugging
npm test -- --inspect-brk

# Run only failed tests
npm test -- --reporter=verbose --run

# Test specific component patterns
npm test -- --grep "Toolbar Component"

# Generate coverage report
npm run test:coverage -- --reporter=html

# Run E2E tests
npm run test:e2e

# Run E2E tests with UI
npm run test:e2e -- --ui

# Debug E2E tests
npm run test:e2e -- --debug
```

### Debugging Commands
```powershell
# Debug Vite build issues
npm run build -- --debug

# Debug with source maps
npm run dev -- --sourcemap

# Analyze bundle size
npm run build -- --analyze

# Check TypeScript errors
npx tsc --noEmit

# Lint and fix issues
npm run lint -- --fix

# Debug Svelte compilation
npx svelte-check --diagnostic

# Profile performance
npm run dev -- --profile
```

## Development Rules

### TDD Cycle
1. **RED**: Write failing test
2. **GREEN**: Minimal code to pass
3. **REFACTOR**: Improve while tests stay green
4. **REPEAT**: Small iterations

### Quality Gates
- All tests pass before new development
- **Tier 1**: Test coverage = 100% for pure functions
- **Tier 2**: Test coverage ≥ 95% for Svelte components
- **Tier 3**: Test coverage ≥ 90% for integration & E2E
- No broken tests committed
- Functions stay under 20 lines
- TypeScript compilation with zero errors

## Project Structure for Svelte 3

```
src/
├── lib/
│   ├── utils/          // Pure functions
│   │   ├── validation.js
│   │   ├── transform.js
│   │   └── calculations.js
│   ├── stores/         // Svelte stores
│   │   ├── signData.js
│   │   └── ui.js
│   ├── components/     // Reusable components
│   │   ├── SignEditor.svelte
│   │   ├── SymbolChooser.svelte
│   │   └── Toolbar.svelte
│   └── services/       // API integration
│       ├── api.js
│       └── storage.js
├── routes/             // SvelteKit routes (if using)
└── app.html
```

## Svelte Component Tree Structure

### Complete Component Hierarchy (Actual Implementation)

Based on the current Svelte implementation, here is the actual component tree structure showing parent-child relationships:

```
App.svelte (Root Application)
├── SignEditor.svelte (Main sign editing canvas)
├── SymbolChooser.svelte (Symbol selection interface)
├── Toolbar.svelte (Editor toolbar with actions)
├── ChooserPanel.svelte (Main chooser panel container)
│   ├── SymbolChooser.svelte (Symbol selection within panel)
│   ├── HandGroupChooser.svelte (Hand-specific symbol groups)
│   ├── GeneralGroupChooser.svelte (General symbol groups)
│   └── Maniquin.svelte (Hand preview/mannequin display)
├── Maniquin.svelte (Standalone hand preview)
└── SymbolSVG.svelte (Individual symbol rendering)
```

### Proper Svelte Structure (Matching Elm Layout)

To properly match the Elm implementation architecture, the component tree should be restructured as follows:

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

This structure provides:
- **Proper layout separation** between left/center/right areas
- **Responsive design** with mobile drawer support
- **Modal system** for dialogs and overlays
- **Clear component hierarchy** matching the original Elm architecture
- **Layout containers** that manage responsive behavior

## Current Implementation Status (Updated June 8, 2025)

### Live Reference Implementation
- **Elm Version**: Available at https://jonathandduncan.github.io/QuickSignEditor/
- **Target Functionality**: Svelte implementation should match Elm version features
- **Comparison Tool**: Use live Elm version to verify feature parity

### Actual Implementation Analysis

The current Svelte implementation is **significantly simpler** than the original Elm structure:

#### Comparison Metrics
- **Elm Implementation**: 76 files across 9 major systems, 7-level deep nesting
- **Svelte Implementation**: ~15 main components, 3-4 level maximum depth  
- **Migration Status**: 65% fully implemented, 25% partial, 10% planned

### Recent Progress (June 2025)
- ✅ **Selection Tools Framework**: EditorMode types, SelectionToolbar component
- 🚧 **Rectangle Selection**: Component created, integration in progress
- 🚧 **Circle Selection**: Component created, needs testing
- 🚧 **Hand Tool (Pan)**: Component created, needs integration
- 📝 **SignEditor Integration**: Selection tools being integrated into main editor

#### Current Working Components
```
src/
├── App.svelte                    # ✅ Complete application shell
├── lib/
│   ├── ChooserPanel.svelte       # 🚧 Layout complete, needs integration
│   ├── GeneralGroupChooser.svelte # 📝 Stub implementation
│   ├── HandGroupChooser.svelte   # 📝 Stub implementation  
│   ├── Maniquin.svelte           # ✅ KEY TARGET - Full FSW rendering
│   ├── SignEditor.svelte         # 🚧 Full editing functionality + selection tools
│   ├── SymbolChooser.svelte      # ✅ Working symbol selection
│   ├── SymbolSVG.svelte          # ✅ Symbol display utility
│   ├── Toolbar.svelte            # ✅ All toolbar actions
│   ├── signStore.ts              # ✅ Centralized state management
│   ├── signWritingFont.ts        # ✅ SignWriting utilities
│   ├── types/
│   │   └── editorTypes.ts        # ✅ NEW - Editor mode types
│   └── components/
│       ├── SelectionToolbar.svelte # ✅ NEW - Tool selection interface
│       ├── RectangleSelect.svelte  # ✅ NEW - Rectangle selection tool
│       ├── CircleSelect.svelte     # ✅ NEW - Circle selection tool
│       └── HandCursor.svelte       # ✅ NEW - Pan/navigation tool
```

### Selection Tools Implementation (NEW - January 2025)

#### Completed Components
1. **EditorTypes**: TypeScript interfaces for editor modes and selection areas
2. **SelectionToolbar**: Tool picker with keyboard shortcuts (V, R, C, H)
3. **RectangleSelect**: Drag-to-select rectangle with intersection detection
4. **CircleSelect**: Drag-to-select circle with radius-based selection
5. **HandCursor**: Pan/zoom navigation with keyboard arrow support

#### Editor Modes Available
- `'select'` - Default selection mode (V key)
- `'rectangleSelect'` - Rectangle selection (R key)
- `'circleSelect'` - Circle selection (C key)  
- `'handCursor'` - Pan/navigation mode (H key)

#### Integration Status
- 🚧 **SignEditor Integration**: Selection components created, template integration in progress
- 📝 **Event Handling**: Mode switching and selection area calculations implemented
- 📝 **Testing**: Need comprehensive tests for all selection tools
- 📝 **UI Polish**: Visual feedback and cursor states need refinement
- 📝 **Feature Verification**: Compare with live Elm version behavior

#### Next Steps
1. Complete SignEditor template integration with selection tools
2. Test selection functionality with existing symbol selection patterns
3. Implement proper cursor states for each editor mode
4. Add comprehensive tests for all selection components
5. Verify feature parity with live Elm version at https://jonathandduncan.github.io/QuickSignEditor/

### Mannequin Component Success Story

The `Maniquin.svelte` component represents a **successful migration** from Elm's `Choosers.Maniquin.elm`:

#### Key Features Migrated
- **Complete FSW Rendering**: Full SignWriting string processing
- **Interactive Symbol Placement**: Body diagram coordinate system
- **Dual-Font Support**: SuttonSignWriting fill + line fonts
- **Responsive Scaling**: Automatic size adaptation
- **Live Data Integration**: Connected to `signStore.signModel`

### Component Reuse Patterns

#### Shared Components
- **SymbolChooser.svelte**: Used in both `App.svelte` and `ChooserPanel.svelte`
- **Maniquin.svelte**: Standalone preview + integrated chooser display
- **SymbolSVG.svelte**: Universal symbol rendering utility

This comprehensive guide provides a concrete starting point for migrating from the Elm architecture to Svelte while maintaining the same functionality and user experience. The component tree structure shows exactly how to organize the codebase, and the implementation examples provide working code patterns for each major component type.
