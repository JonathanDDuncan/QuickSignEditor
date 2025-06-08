# QuickSignEditor 2025 - Svelte 3 Implementation Guide

## Project Overview

This implementation guide provides the technical roadmap for QuickSignEditor 2025, a modern rewrite of the legacy Elm-based SignWriting editor as a Svelte 3 application. The project emphasizes maintainability, comprehensive testing, and AI-optimized development patterns.

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

### Testing-First Development Philosophy
- **Red-Green-Refactor**: Write failing tests first, implement minimal code, then refactor
- **Test Isolation**: Each test should be independent and atomic
- **Mock Strategically**: Mock external dependencies but not the code under test
- **Debug Incrementally**: Use console.log, debugger statements, and test-driven debugging

### Three-Tier Development Approach

#### Tier 1: Pure Functions (JavaScript/TypeScript)
Write and test business logic without side effects:
```javascript
// Good: Pure function
export function validateSignData(signData) {
  if (!signData?.name) return { isValid: false, error: 'Name required' };
  if (!signData?.symbols) return { isValid: false, error: 'Symbols required' };
  return { isValid: true };
}

// Test
import { validateSignData } from './validation.js';
test('validates sign data correctly', () => {
  expect(validateSignData({ name: 'test', symbols: [] })).toEqual({ isValid: true });
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

### Core Component Architecture

#### SignEditor.svelte (Main Canvas Component)
- Drag and drop functionality for symbols
- Canvas rendering and manipulation
- Symbol positioning and transformation
- Integration with undo/redo system

#### SymbolChooser.svelte (Symbol Library)
- Symbol browsing and filtering
- Search functionality
- Category-based organization
- Symbol preview and selection

#### Toolbar.svelte (Action Controls)
- Undo/redo buttons
- Copy/paste/align operations
- FSW import/export controls
- Canvas manipulation tools

## Testing Strategy

### Tier 1: Pure Functions (100% coverage required)
```javascript
// validation.test.js
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { validateSignData } from '../lib/utils/validation.js';

describe('validateSignData', () => {
  // Debugging helper for complex test scenarios
  const debugLog = (label, data) => {
    if (process.env.NODE_ENV === 'test' && process.env.DEBUG_TESTS) {
      console.log(`[DEBUG] ${label}:`, JSON.stringify(data, null, 2));
    }
  };

  beforeEach(() => {
    // Reset any global state
    vi.clearAllMocks();
  });

  afterEach(() => {
    // Clean up after each test
    vi.restoreAllMocks();
  });

  it('returns valid for complete data', () => {
    const input = { name: 'test', symbols: ['symbol1'] };
    debugLog('Input', input);
    
    const result = validateSignData(input);
    debugLog('Result', result);
    
    expect(result.isValid).toBe(true);
    expect(result).toMatchSnapshot(); // Snapshot testing for complex objects
  });
  
  it('returns invalid for missing name', () => {
    const input = { symbols: ['symbol1'] };
    const result = validateSignData(input);
    
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('Name required');
  });

  // Property-based testing for edge cases
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

  // Performance testing for large datasets
  it('performs well with large symbol arrays', () => {
    const largeSymbolArray = Array(1000).fill().map((_, i) => `symbol${i}`);
    const input = { name: 'test', symbols: largeSymbolArray };
    
    const start = performance.now();
    const result = validateSignData(input);
    const end = performance.now();
    
    expect(result.isValid).toBe(true);
    expect(end - start).toBeLessThan(10); // Should complete in under 10ms
  });
});
```

### Tier 2: Component Testing (95% coverage required)
```javascript
// SignEditor.test.js
import { render, screen, fireEvent, waitFor, cleanup } from '@testing-library/svelte';
import { vi, describe, it, expect, beforeEach, afterEach } from 'vitest';
import SignEditor from '../lib/components/SignEditor.svelte';

// Mock stores with proper Svelte store interface
vi.mock('../lib/stores', () => {
  const makeMockStore = (initialValue) => ({
    subscribe: vi.fn((callback) => {
      callback(initialValue);
      return () => {}; // Unsubscribe function
    }),
    set: vi.fn(),
    update: vi.fn(),
  });

  return {
    signData: makeMockStore({ symbols: [], name: '' }),
    ui: {
      ...makeMockStore({ tool: 'select', zoom: 1 }),
      setTool: vi.fn(),
      setZoom: vi.fn(),
    },
  };
});

describe('SignEditor Component', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  afterEach(() => {
    cleanup();
  });

  it('displays error for invalid data', async () => {
    const { component } = render(SignEditor, { 
      props: { signData: {} } 
    });
    
    await waitFor(() => {
      expect(screen.getByText('Name required')).toBeInTheDocument();
    });
    
    // Component debugging
    if (process.env.DEBUG_TESTS) {
      console.log('Component HTML:', component.$$.root.innerHTML);
    }
  });

  it('handles user interactions correctly', async () => {
    const { component } = render(SignEditor, {
      props: { 
        signData: { name: 'test', symbols: [] }
      }
    });

    const canvas = screen.getByRole('img', { name: /sign canvas/i });
    
    // Simulate drag and drop
    await fireEvent.mouseDown(canvas, { clientX: 100, clientY: 100 });
    await fireEvent.mouseMove(canvas, { clientX: 150, clientY: 150 });
    await fireEvent.mouseUp(canvas, { clientX: 150, clientY: 150 });
    
    // Verify state changes
    expect(component.$$.props.signData.symbols).toHaveLength(1);
  });

  // Test Svelte-specific reactivity
  it('updates when props change', async () => {
    const { component } = render(SignEditor, {
      props: { signData: { name: 'initial', symbols: [] } }
    });

    // Update props
    await component.$set({ 
      signData: { name: 'updated', symbols: [{ id: '1', x: 0, y: 0 }] } 
    });

    await waitFor(() => {
      expect(screen.getByText('updated')).toBeInTheDocument();
    });
  });

  // Error boundary testing
  it('handles errors gracefully', async () => {
    const consoleError = vi.spyOn(console, 'error').mockImplementation(() => {});
    
    render(SignEditor, {
      props: { signData: null } // Invalid props
    });

    expect(consoleError).toHaveBeenCalled();
    consoleError.mockRestore();
  });
});
```

### Store Testing Patterns
```javascript
// ui.test.js
import { get } from 'svelte/store';
import { ui, selectedSymbol, currentTool } from '../lib/stores/ui.js';

describe('UI Store', () => {
  beforeEach(() => {
    ui.reset(); // Reset to default state
  });

  it('maintains state consistency', () => {
    ui.setSelectedSymbol('symbol1');
    ui.setTool('hand');
    
    // Test derived store updates
    expect(get(selectedSymbol)).toBe('symbol1');
    expect(get(currentTool)).toBe('hand');
    
    // Test store subscription
    let capturedValue;
    const unsubscribe = ui.subscribe(value => {
      capturedValue = value;
    });
    
    expect(capturedValue.selectedSymbol).toBe('symbol1');
    expect(capturedValue.tool).toBe('hand');
    
    unsubscribe();
  });

  it('handles invalid state updates', () => {
    const initialState = get(ui);
    
    // Try to set invalid tool
    ui.setTool('invalid-tool');
    
    // Should fall back to default
    expect(get(currentTool)).toBe('select');
  });
});
```

### Tier 3: Integration Testing (90% coverage required)
```javascript
// integration.test.js
import { test, expect, Page } from '@playwright/test';

// Custom test fixtures for better debugging
const testWithDebug = test.extend({
  page: async ({ page }, use) => {
    // Add console logging in debug mode
    if (process.env.DEBUG_E2E) {
      page.on('console', msg => console.log(`PAGE LOG: ${msg.text()}`));
      page.on('pageerror', error => console.error(`PAGE ERROR: ${error.message}`));
    }
    await use(page);
  },
});

testWithDebug.describe('Complete sign creation workflow', () => {
  testWithDebug.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
  });

  testWithDebug('creates and saves a sign', async ({ page }) => {
    // Take screenshot at start for debugging
    if (process.env.DEBUG_E2E) {
      await page.screenshot({ path: 'debug-start.png' });
    }

    await page.fill('[data-testid="sign-name"]', 'My Sign');
    await page.click('[data-testid="add-symbol"]');
    
    // Wait for symbol chooser to appear
    await page.waitForSelector('[data-testid="symbol-chooser"]', { state: 'visible' });
    
    // Select a symbol
    await page.click('[data-testid="symbol-S10000"]');
    
    // Wait for symbol to appear on canvas
    await page.waitForSelector('[data-testid="canvas-symbol"]');
    
    // Save the sign
    await page.click('[data-testid="save-sign"]');
    
    // Verify success
    await expect(page.locator('[data-testid="success-message"]')).toBeVisible();
    
    // Take final screenshot
    if (process.env.DEBUG_E2E) {
      await page.screenshot({ path: 'debug-success.png' });
    }
  });

  testWithDebug('handles errors gracefully', async ({ page }) => {
    // Try to save without a name
    await page.click('[data-testid="save-sign"]');
    
    await expect(page.locator('[data-testid="error-message"]')).toContainText('Name required');
  });

  // Performance testing
  testWithDebug('loads quickly', async ({ page }) => {
    const startTime = Date.now();
    await page.goto('/');
    await page.waitForSelector('[data-testid="main-toolbar"]');
    const loadTime = Date.now() - startTime;
    
    expect(loadTime).toBeLessThan(3000); // Should load in under 3 seconds
  });
});
```

### Mock Strategy Best Practices
```javascript
// Mock external dependencies but not internal logic
vi.mock('external-library', () => ({
  externalFunction: vi.fn(() => 'mocked result'),
}));

// Don't mock the code under test
// ❌ Bad
vi.mock('../lib/utils/validation.js');

// ✅ Good - test the real validation logic
import { validateSignData } from '../lib/utils/validation.js';

// Mock Svelte stores properly
vi.mock('../lib/stores', () => {
  const createMockStore = (initialValue) => {
    let currentValue = initialValue;
    const subscribers = new Set();
    
    return {
      subscribe: vi.fn((callback) => {
        callback(currentValue);
        subscribers.add(callback);
        return () => subscribers.delete(callback);
      }),
      set: vi.fn((value) => {
        currentValue = value;
        subscribers.forEach(callback => callback(value));
      }),
      update: vi.fn((updater) => {
        currentValue = updater(currentValue);
        subscribers.forEach(callback => callback(currentValue));
      }),
    };
  };

  return {
    ui: createMockStore({ tool: 'select', zoom: 1 }),
    signData: createMockStore({ symbols: [], name: '' }),
  };
});
```

## Implementation Checkpoints

### Tier 1 Complete
- [ ] All business logic extracted to pure functions
- [ ] 100% test coverage for pure functions
- [ ] Functions under 20 lines each
- [ ] TypeScript types defined

### Tier 2 Complete  
- [ ] Svelte components created and tested
- [ ] Components use pure functions for logic
- [ ] 95% component test coverage
- [ ] Stores implemented for state management
- [ ] SignEditor, SymbolChooser, and Toolbar components
- [ ] Responsive design implementation

### Tier 3 Complete
- [ ] Full user workflows implemented
- [ ] E2E tests covering main user journeys
- [ ] 90% overall test coverage
- [ ] Performance validated
- [ ] Accessibility compliance verified
- [ ] Cross-browser compatibility confirmed

## Common Svelte 3 Patterns

### Pure Function Integration
```svelte
<script>
  import { validateSignData, transformSymbols } from '$lib/utils';
  
  export let signData;
  
  // Reactive declarations use pure functions
  $: validation = validateSignData(signData);
  $: processedSymbols = transformSymbols(signData.symbols);
</script>
```

### Store Pattern
```javascript
// stores/signData.js
import { writable } from 'svelte/store';
import { validateSignData } from '../utils/validation.js';

function createSignStore() {
  const { subscribe, set, update } = writable({});
  
  return {
    subscribe,
    setSignData: (data) => set(data),
    validate: () => get(signStore).pipe(validateSignData)
  };
}

export const signStore = createSignStore();
```

## User Stories & Development Focus

### Primary User Workflows
- **Sign Creation**: Users can create and edit signs using a drag-and-drop interface
- **Symbol Management**: Users can search, browse, and filter symbols efficiently
- **History Control**: Users can undo and redo their actions at any time
- **Content Operations**: Users can copy, paste, and align symbols or groups
- **Format Integration**: Users can import and export signs in FSW format
- **Cross-Platform Access**: Users can use the editor on any device (desktop, tablet, mobile)
- **Accessibility Support**: Users can navigate and operate the app using only the keyboard
- **Help & Documentation**: Users can access help and documentation easily

### Target User Groups
- Sign language educators and students
- Deaf community members
- Linguists and researchers
- Anyone needing to create, edit, or share SignWriting symbols

## Performance & Security Requirements

### Performance Standards
- Fast load times and smooth interactions
- Optimized bundle size with tree shaking
- Efficient rendering with Svelte's reactive updates

### Security Considerations
- Safe handling of user data and files
- Client-side processing to minimize data exposure
- Secure FSW format parsing and validation

## Setup and Dependencies

### Required Tools and Dependencies
```powershell
# Install Node.js and npm
npm install -g @sveltejs/cli

# Core TypeScript and Svelte setup
npm install -D typescript @tsconfig/svelte svelte-check
npm install -D @types/node

# Testing framework setup with Vitest (recommended for Svelte)
npm install -D vitest @testing-library/svelte @testing-library/jest-dom jsdom
npm install -D @vitest/ui @vitest/coverage-v8
npm install -D @playwright/test  # For E2E testing

# Code quality tools
npm install -D eslint @typescript-eslint/parser @typescript-eslint/eslint-plugin
npm install -D eslint-plugin-svelte3 eslint-config-prettier
npm install -D prettier prettier-plugin-svelte

# Mocking and test utilities
npm install -D msw whatwg-fetch
npm install -D @testing-library/user-event

# Development tools
npm install -D @sveltejs/vite-plugin-svelte
npm install -D vite-plugin-eslint
```

### TypeScript Configuration
```json
// tsconfig.json
{
  "extends": "@tsconfig/svelte/tsconfig.json",
  "compilerOptions": {
    "target": "ES2020",
    "useDefineForClassFields": true,
    "lib": ["ES2020", "DOM", "DOM.Iterable"],
    "module": "ESNext",
    "skipLibCheck": true,
    "moduleResolution": "bundler",
    "allowImportingTsExtensions": true,
    "resolveJsonModule": true,
    "isolatedModules": true,
    "noEmit": true,
    "allowJs": true,
    "checkJs": true,
    "strict": true,
    "noUnusedLocals": true,
    "noUnusedParameters": true,
    "noFallthroughCasesInSwitch": true,
    "baseUrl": ".",
    "paths": {
      "$lib": ["./src/lib"],
      "$lib/*": ["./src/lib/*"]
    }
  },
  "include": ["src/**/*.d.ts", "src/**/*.ts", "src/**/*.js", "src/**/*.svelte"],
  "references": [{ "path": "./tsconfig.node.json" }]
}
```

### ESLint Configuration
```javascript
// .eslintrc.js
module.exports = {
  parser: '@typescript-eslint/parser',
  extends: [
    'eslint:recommended',
    '@typescript-eslint/recommended',
    'prettier'
  ],
  plugins: ['svelte3', '@typescript-eslint'],
  ignorePatterns: ['*.cjs'],
  overrides: [
    { 
      files: ['*.svelte'], 
      processor: 'svelte3/svelte3' 
    }
  ],
  settings: {
    'svelte3/typescript': () => require('typescript')
  },
  parserOptions: {
    sourceType: 'module',
    ecmaVersion: 2020
  },
  env: {
    browser: true,
    es2017: true,
    node: true
  },
  rules: {
    // TypeScript specific
    '@typescript-eslint/no-unused-vars': ['error', { argsIgnorePattern: '^_' }],
    '@typescript-eslint/no-explicit-any': 'warn',
    '@typescript-eslint/explicit-function-return-type': 'off',
    
    // General code quality
    'no-console': process.env.NODE_ENV === 'production' ? 'error' : 'warn',
    'no-debugger': process.env.NODE_ENV === 'production' ? 'error' : 'warn',
    'prefer-const': 'error',
    'no-var': 'error'
  }
};
```

### Prettier Configuration
```javascript
// .prettierrc.js
module.exports = {
  useTabs: false,
  singleQuote: true,
  trailingComma: 'es5',
  printWidth: 100,
  plugins: ['prettier-plugin-svelte'],
  pluginSearchDirs: ['.'],
  overrides: [
    { 
      files: '*.svelte', 
      options: { 
        parser: 'svelte' 
      } 
    }
  ]
};
```

### Vitest Configuration
```javascript
// vitest.config.js
import { defineConfig } from 'vitest/config';
import { svelte } from '@sveltejs/vite-plugin-svelte';

export default defineConfig({
  plugins: [svelte({ hot: !process.env.VITEST })],
  test: {
    globals: true,
    environment: 'jsdom',
    setupFiles: ['./src/tests/setup.js'],
    include: ['src/**/*.test.{js,ts}'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        'coverage/**',
        'dist/**',
        '**/*.d.ts',
        '**/*.test.{ts,js}',
        'test{,s}/**',
        '**/__tests__/**'
      ],
      thresholds: {
        global: {
          branches: 85,
          functions: 90,
          lines: 90,
          statements: 90
        },
        './src/lib/utils/**/*.js': {
          branches: 100,
          functions: 100,
          lines: 100,
          statements: 100
        }
      }
    },
    testTimeout: 10000,
    hookTimeout: 10000
  },
  resolve: {
    alias: {
      '$lib': new URL('./src/lib', import.meta.url).pathname
    }
  }
});
```

### Package.json Scripts
```json
{
  "scripts": {
    "dev": "vite dev",
    "build": "vite build",
    "preview": "vite preview",
    
    "check": "svelte-check --tsconfig ./tsconfig.json",
    "check:watch": "svelte-check --tsconfig ./tsconfig.json --watch",
    
    "lint": "eslint src --ext .js,.ts,.svelte",
    "lint:fix": "eslint src --ext .js,.ts,.svelte --fix",
    "format": "prettier --write src",
    "format:check": "prettier --check src",
    
    "test": "vitest",
    "test:run": "vitest run",
    "test:ui": "vitest --ui",
    "test:coverage": "vitest run --coverage",
    "test:watch": "vitest --watch",
    "test:debug": "vitest --inspect-brk",
    "test:verbose": "vitest --reporter=verbose",
    "test:failed": "vitest run --reporter=verbose --bail=1",
    
    "test:e2e": "playwright test",
    "test:e2e:ui": "playwright test --ui",
    "test:e2e:debug": "playwright test --debug",
    
    "typecheck": "tsc --noEmit",
    "validate": "npm run lint && npm run typecheck && npm run test:run",
    "validate:ci": "npm run format:check && npm run lint && npm run typecheck && npm run test:coverage && npm run build",
    
    "clean": "rm -rf dist coverage .nyc_output",
    "analyze": "npm run build -- --analyze",
    "security-check": "npm audit --audit-level=high"
  },
  "devDependencies": {
    "@sveltejs/vite-plugin-svelte": "^3.0.0",
    "@testing-library/svelte": "^4.0.0",
    "@testing-library/jest-dom": "^6.0.0",
    "@testing-library/user-event": "^14.0.0",
    "@playwright/test": "^1.40.0",
    "@vitest/ui": "^1.0.0",
    "@vitest/coverage-v8": "^1.0.0",
    "vitest": "^1.0.0",
    "jsdom": "^23.0.0",
    "msw": "^2.0.0",
    "whatwg-fetch": "^3.6.0",
    
    "typescript": "^5.0.0",
    "@tsconfig/svelte": "^5.0.0",
    "svelte-check": "^3.5.0",
    "@types/node": "^20.0.0",
    
    "eslint": "^8.50.0",
    "@typescript-eslint/parser": "^6.0.0",
    "@typescript-eslint/eslint-plugin": "^6.0.0",
    "eslint-plugin-svelte3": "^4.0.0",
    "eslint-config-prettier": "^9.0.0",
    
    "prettier": "^3.0.0",
    "prettier-plugin-svelte": "^3.0.0",
    
    "vite": "^5.0.0",
    "vite-plugin-eslint": "^1.8.0"
  }
}
```

## Development Milestones & Phases

### Phase 1: Foundation (Tier 1 Complete)
- [ ] All business logic extracted to pure functions
- [ ] 100% test coverage for pure functions  
- [ ] Functions under 20 lines each
- [ ] TypeScript types defined
- [ ] FSW parsing and validation functions
- [ ] Core calculation and transformation utilities

### Phase 2: Component Development (Tier 2 Complete)
- [ ] Svelte components created and tested
- [ ] Components use pure functions for logic
- [ ] 95% component test coverage
- [ ] Stores implemented for state management
- [ ] SignEditor, SymbolChooser, and Toolbar components
- [ ] Responsive design implementation

### Phase 3: Integration & Polish (Tier 3 Complete)
- [ ] Full user workflows implemented
- [ ] E2E tests covering main user journeys
- [ ] 90% overall test coverage
- [ ] Performance validated
- [ ] Accessibility compliance verified
- [ ] Cross-browser compatibility confirmed

### Phase 4: Documentation & Deployment
- [ ] Comprehensive user documentation
- [ ] Developer guides and API documentation
- [ ] Deployment pipeline established
- [ ] Performance monitoring setup

## Quality Assurance & Testing Framework

### Acceptance Criteria
- All legacy features are present and functionally tested
- FSW format compatibility maintained with legacy system
- Performance meets or exceeds legacy application
- App passes accessibility audits (WCAG 2.1 AA)
- Works reliably on all major browsers and devices
- TypeScript compilation with zero errors

### Development Standards
- TDD cycle followed for all new features
- Pure functions separated from UI logic
- Component isolation verified through testing
- Code review process ensures quality gates

## Debugging & Troubleshooting Guide

### Common Testing Issues & Solutions

#### Issue: "Cannot read properties of undefined"
```javascript
// Problem: Mocked store doesn't match expected interface
// ❌ Bad mock
const ui = { subscribe: vi.fn() };

// ✅ Good mock - matches Svelte store interface
const ui = {
  subscribe: vi.fn((callback) => {
    callback({ tool: 'select', zoom: 1 });
    return () => {};
  }),
  set: vi.fn(),
  update: vi.fn(),
  // Include all expected properties
  tool: { subscribe: vi.fn(), set: vi.fn() },
  currentTool: { subscribe: vi.fn() },
};
```

#### Issue: "vi.mock is not working"
```javascript
// Problem: Mock is defined after imports
import Component from './Component.svelte'; // ❌ This runs before mock
vi.mock('./store.js');

// Solution: Hoist mocks to top of file
vi.mock('./store.js', () => ({ /* mock */ }));
import Component from './Component.svelte'; // ✅ Now mock is active
```

#### Issue: "Tests fail randomly"
```javascript
// Problem: Tests share state
let globalVariable = {};

// Solution: Reset state in beforeEach
beforeEach(() => {
  vi.clearAllMocks();
  vi.resetAllMocks();
  globalVariable = {};
  // Reset any global state
});
```

### Debugging Svelte Components

#### Debug Reactive Statements
```svelte
<script>
  let count = 0;
  
  // Add debugging to reactive statements
  $: {
    console.log('Count changed:', count);
    debugger; // Breaks in browser dev tools
  }
  
  $: doubled = count * 2;
  $: {
    console.log('Doubled:', doubled);
  }
</script>
```

#### Debug Store Subscriptions
```javascript
// stores/debug.js
import { writable } from 'svelte/store';

export function createDebugStore(name, initialValue) {
  const { subscribe, set, update } = writable(initialValue);
  
  return {
    subscribe: (callback) => {
      console.log(`[${name}] Subscribing`);
      return subscribe((value) => {
        console.log(`[${name}] Value changed:`, value);
        callback(value);
      });
    },
    set: (value) => {
      console.log(`[${name}] Setting:`, value);
      set(value);
    },
    update: (updater) => {
      console.log(`[${name}] Updating`);
      update(updater);
      console.log(`[${name}] Update: ${JSON.stringify(current)} → ${JSON.stringify(next)}`);
    },
  };
}
```

#### Debug Component Props and Events
```svelte
<!-- Component.svelte -->
<script>
  export let prop1;
  export let prop2;
  
  // Debug prop changes
  $: console.log('Props changed:', { prop1, prop2 });
  
  function handleClick() {
    console.log('Click event fired');
    console.trace(); // Shows call stack
  }
</script>
```

### Performance Debugging

#### Measure Component Render Time
```javascript
// utils/performance.js
export function measureRender(componentName, renderFunction) {
  const start = performance.now();
  const result = renderFunction();
  const end = performance.now();
  
  console.log(`[PERF] ${componentName} rendered in ${end - start}ms`);
  return result;
}

// In test
import { measureRender } from './utils/performance.js';

test('component renders quickly', () => {
  const { container } = measureRender('SignEditor', () => 
    render(SignEditor, { props: { signData: mockData } })
  );
  
  expect(container).toBeInTheDocument();
});
```

#### Profile Store Updates
```javascript
// stores/ui.js with profiling
export const ui = (() => {
  const { subscribe, set, update } = writable(defaultState);
  
  return {
    subscribe,
    set: (value) => {
      console.time('ui.set');
      set(value);
      console.timeEnd('ui.set');
    },
    update: (updater) => {
      console.time('ui.update');
      update(updater);
      console.timeEnd('ui.update');
    },
  };
})();
```

### Test Debugging Utilities

#### Custom Test Matchers
```javascript
// test-utils/matchers.js
import { expect } from 'vitest';

expect.extend({
  toBeValidSignData(received) {
    const pass = received && received.name && Array.isArray(received.symbols);
    return {
      pass,
      message: () => 
        pass 
          ? `Expected ${JSON.stringify(received)} not to be valid sign data`
          : `Expected ${JSON.stringify(received)} to be valid sign data`
    };
  },
});

// Usage in tests
expect(signData).toBeValidSignData();
```

#### Debug Test Environment
```javascript
// test-utils/debug.js
export function debugTest(testName, debugFn) {
  if (process.env.DEBUG_TESTS) {
    console.group(`[DEBUG] ${testName}`);
    debugFn();
    console.groupEnd();
  }
}

// Usage
test('validates data', () => {
  const input = { name: 'test' };
  
  debugTest('Input validation', () => {
    console.log('Input:', input);
    console.log('Environment:', process.env.NODE_ENV);
  });
  
  const result = validateSignData(input);
  expect(result.isValid).toBe(false);
});
```

### Browser Debugging

#### Debug Canvas Interactions
```javascript
// components/SignEditor.svelte
<script>
  function handleCanvasClick(event) {
    // Debug coordinates
    const rect = event.target.getBoundingClientRect();
    const x = event.clientX - rect.left;
    const y = event.clientY - rect.top;
    
    console.log('Canvas click:', { x, y, event });
    
    // Visual debugging
    if (process.env.NODE_ENV === 'development') {
      const debugDot = document.createElement('div');
      debugDot.style.cssText = `
        position: absolute;
        left: ${event.clientX}px;
        top: ${event.clientY}px;
        width: 4px;
        height: 4px;
        background: red;
        z-index: 9999;
        pointer-events: none;
      `;
      document.body.appendChild(debugDot);
      setTimeout(() => debugDot.remove(), 2000);
    }
  }
</script>
```

#### Debug Store State in Browser
```javascript
// Add to main app for development
if (typeof window !== 'undefined' && process.env.NODE_ENV === 'development') {
  import('./lib/stores').then(stores => {
    window.debugStores = stores;
    console.log('Debug stores available at window.debugStores');
  });
}

// In browser console:
// debugStores.ui.subscribe(console.log)
// debugStores.signData.set({ name: 'test', symbols: [] })
```

### Error Handling Patterns

#### Graceful Component Error Handling
```svelte
<!-- ErrorBoundary.svelte -->
<script>
  export let fallback = 'Something went wrong';
  let error = null;
  
  function handleError(event) {
    error = event.detail;
    console.error('Component error:', error);
  }
</script>

{#if error}
  <div class="error-boundary">
    <h3>Error</h3>
    <p>{fallback}</p>
    {#if process.env.NODE_ENV === 'development'}
      <details>
        <summary>Error Details</summary>
        <pre>{JSON.stringify(error, null, 2)}</pre>
      </details>
    {/if}
  </div>
{:else}
  <div on:error={handleError}>
    <slot />
  </div>
{/if}
```

#### Async Error Handling
```javascript
// utils/async.js
export async function safeAsync(asyncFn, fallback = null) {
  try {
    return await asyncFn();
  } catch (error) {
    console.error('Async operation failed:', error);
    return fallback;
  }
}

// Usage
const result = await safeAsync(
  () => fetch('/api/signs').then(r => r.json()),
  []
);
```

## TypeScript Integration & Type Safety

### TypeScript Configuration for Better Testing
```typescript
// types/test.d.ts
declare global {
  namespace Vi {
    interface Assertion<T = any> {
      toBeValidSignData(): T;
      toHaveNoErrors(): T;
      toMatchStoreShape(): T;
    }
  }
  
  var debugTest: (label: string, data: any) => void;
}

// Component prop types
export interface SignData {
  name: string;
  symbols: Symbol[];
  metadata?: Record<string, any>;
}

export interface Symbol {
  id: string;
  code: string;
  x: number;
  y: number;
  rotation?: number;
  fill?: string;
  selected?: boolean;
}

// Store types
export interface UIState {
  selectedSymbol: string | null;
  selectedSymbols: string[];
  tool: 'select' | 'hand' | 'zoom' | 'pen';
  zoom: number;
  pan: { x: number; y: number };
  clipboard: Symbol[] | null;
  showGrid: boolean;
  snapToGrid: boolean;
}

// Test helper types
export interface MockStore<T> {
  subscribe: (callback: (value: T) => void) => () => void;
  set: (value: T) => void;
  update: (updater: (current: T) => T) => void;
}
```

### Strongly Typed Test Utilities
```typescript
// test-utils/typed-helpers.ts
import type { RenderResult } from '@testing-library/svelte';
import type { SignData, UIState, Symbol } from '../types';

export function createMockSignData(overrides: Partial<SignData> = {}): SignData {
  return {
    name: 'Test Sign',
    symbols: [],
    ...overrides,
  };
}

export function createMockUIState(overrides: Partial<UIState> = {}): UIState {
  return {
    selectedSymbol: null,
    selectedSymbols: [],
    tool: 'select',
    zoom: 1,
    pan: { x: 0, y: 0 },
    clipboard: null,
    showGrid: true,
    snapToGrid: true,
    ...overrides,
  };
}

export function createMockSymbol(overrides: Partial<Symbol> = {}): Symbol {
  return {
    id: 'mock-symbol',
    code: 'S10000',
    x: 0,
    y: 0,
    ...overrides,
  };
}

// Type-safe component testing
export async function renderWithProps<T extends Record<string, any>>(
  Component: any,
  props: T
): Promise<RenderResult<any>> {
  return render(Component, { props });
}

// Type-safe event simulation
export interface CanvasInteraction {
  type: 'click' | 'drag' | 'wheel';
  x: number;
  y: number;
  deltaY?: number;
  buttons?: number;
}

export async function simulateCanvasInteraction(
  canvas: HTMLElement,
  interaction: CanvasInteraction
): Promise<void> {
  const rect = canvas.getBoundingClientRect();
  const clientX = rect.left + interaction.x;
  const clientY = rect.top + interaction.y;

  switch (interaction.type) {
    case 'click':
      await fireEvent.click(canvas, { clientX, clientY });
      break;
    case 'drag':
      await fireEvent.mouseDown(canvas, { clientX, clientY });
      await fireEvent.mouseMove(canvas, { 
        clientX: clientX + 10, 
        clientY: clientY + 10 
      });
      await fireEvent.mouseUp(canvas, { 
        clientX: clientX + 10, 
        clientY: clientY + 10 
      });
      break;
    case 'wheel':
      await fireEvent.wheel(canvas, { 
        clientX, 
        clientY, 
        deltaY: interaction.deltaY || 0 
      });
      break;
  }
}
```

### Advanced Testing Patterns

#### Property-Based Testing
```typescript
// utils/property-testing.ts
export function generateRandomSignData(): SignData {
  const symbolCount = Math.floor(Math.random() * 10) + 1;
  const symbols = Array.from({ length: symbolCount }, (_, i) => ({
    id: `symbol-${i}`,
    code: `S${10000 + Math.floor(Math.random() * 90000)}`,
    x: Math.floor(Math.random() * 500),
    y: Math.floor(Math.random() * 500),
  }));

  return {
    name: `Test Sign ${Math.floor(Math.random() * 1000)}`,
    symbols,
  };
}

// Property-based test example
it.each(Array.from({ length: 100 }, () => generateRandomSignData()))(
  'validates any valid sign data structure',
  (signData) => {
    const result = validateSignData(signData);
    expect(result.isValid).toBe(true);
  }
);
```

#### Mutation Testing Patterns
```typescript
// test-utils/mutations.ts
export function createMutations<T>(original: T): T[] {
  const mutations: T[] = [];
  
  // Create various mutations of the original object
  if (typeof original === 'object' && original !== null) {
    for (const key in original) {
      // Null mutation
      mutations.push({ ...original, [key]: null });
      
      // Undefined mutation
      mutations.push({ ...original, [key]: undefined });
      
      // Type-specific mutations
      const value = original[key];
      if (typeof value === 'string') {
        mutations.push({ ...original, [key]: '' });
        mutations.push({ ...original, [key]: 'invalid' });
      } else if (typeof value === 'number') {
        mutations.push({ ...original, [key]: -1 });
        mutations.push({ ...original, [key]: Infinity });
        mutations.push({ ...original, [key]: NaN });
      } else if (Array.isArray(value)) {
        mutations.push({ ...original, [key]: [] });
        mutations.push({ ...original, [key]: null });
      }
    }
  }
  
  return mutations;
}

// Usage in tests
describe('validateSignData resilience', () => {
  const validSignData = createMockSignData();
  const mutations = createMutations(validSignData);
  
  it.each(mutations)('handles mutations gracefully', (mutated) => {
    expect(() => validateSignData(mutated)).not.toThrow();
  });
});
```

### Test Data Management

#### Fixture Management
```typescript
// fixtures/sign-data.ts
export const FIXTURE_SIGNS = {
  EMPTY: createMockSignData({ symbols: [] }),
  SINGLE_SYMBOL: createMockSignData({
    symbols: [createMockSymbol({ id: 'single', code: 'S10000' })]
  }),
  MULTIPLE_SYMBOLS: createMockSignData({
    symbols: [
      createMockSymbol({ id: 'first', code: 'S10000', x: 100, y: 100 }),
      createMockSymbol({ id: 'second', code: 'S20000', x: 200, y: 150 }),
    ]
  }),
  COMPLEX: createMockSignData({
    name: 'Complex Sign',
    symbols: Array.from({ length: 10 }, (_, i) => 
      createMockSymbol({ 
        id: `symbol-${i}`, 
        x: i * 50, 
        y: i * 30 
      })
    )
  }),
};

// Usage
test('handles empty signs', () => {
  const result = validateSignData(FIXTURE_SIGNS.EMPTY);
  expect(result.isValid).toBe(true);
});
```

#### Snapshot Testing for Complex Objects
```typescript
// tests/snapshots.test.ts
import { FIXTURE_SIGNS } from '../fixtures/sign-data.js';

describe('Sign data structure snapshots', () => {
  it('maintains consistent structure for empty signs', () => {
    expect(FIXTURE_SIGNS.EMPTY).toMatchSnapshot();
  });

  it('maintains consistent structure for complex signs', () => {
    expect(FIXTURE_SIGNS.COMPLEX).toMatchSnapshot();
  });

  it('validates transformation results', () => {
    const transformed = transformSignData(FIXTURE_SIGNS.MULTIPLE_SYMBOLS);
    expect(transformed).toMatchSnapshot();
  });
});
```

## Continuous Integration & Quality Gates

### GitHub Actions Workflow
```yaml
# .github/workflows/test.yml
name: Test & Quality Checks

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v4
    
    - name: Setup Node.js
      uses: actions/setup-node@v4
      with:
        node-version: '18'
        cache: 'npm'
    
    - name: Install dependencies
      run: npm ci
    
    - name: TypeScript check
      run: npm run typecheck
    
    - name: Lint check
      run: npm run lint
    
    - name: Unit tests
      run: npm run test:coverage
    
    - name: Upload coverage reports
      uses: codecov/codecov-action@v3
      with:
        file: ./coverage/coverage-final.json
    
    - name: E2E tests
      run: npm run test:e2e
    
    - name: Build check
      run: npm run build

  quality-gates:
    runs-on: ubuntu-latest
    needs: test
    
    steps:
    - uses: actions/checkout@v4
    
    - name: Setup Node.js
      uses: actions/setup-node@v4
      with:
        node-version: '18'
        cache: 'npm'
    
    - name: Install dependencies
      run: npm ci
    
    - name: Check coverage threshold
      run: |
        npm run test:coverage -- --reporter=json --outputFile=coverage-summary.json
        node scripts/check-coverage.js
    
    - name: Bundle size check
      run: |
        npm run build
        npx bundlesize

  security:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v4
    
    - name: Run security audit
      run: npm audit --audit-level=high
    
    - name: Check for vulnerabilities
      run: npm run security-check
```

### Pre-commit Hooks
```javascript
// scripts/pre-commit.js
#!/usr/bin/env node

const { execSync } = require('child_process');
const fs = require('fs');

function runCommand(command, description) {
  console.log(`Running ${description}...`);
  try {
    execSync(command, { stdio: 'inherit' });
    console.log(`✅ ${description} passed`);
  } catch (error) {
    console.error(`❌ ${description} failed`);
    process.exit(1);
  }
}

function checkCoverage() {
  const coveragePath = './coverage/coverage-summary.json';
  if (!fs.existsSync(coveragePath)) {
    console.error('❌ Coverage report not found');
    process.exit(1);
  }
  
  const coverage = JSON.parse(fs.readFileSync(coveragePath, 'utf8'));
  const thresholds = {
    statements: 90,
    branches: 85,
    functions: 90,
    lines: 90
  };
  
  for (const [metric, threshold] of Object.entries(thresholds)) {
    const actual = coverage.total[metric].pct;
    if (actual < threshold) {
      console.error(`❌ ${metric} coverage ${actual}% is below threshold ${threshold}%`);
      process.exit(1);
    }
  }
  
  console.log('✅ Coverage thresholds met');
}

// Run quality checks
runCommand('npm run typecheck', 'TypeScript check');
runCommand('npm run lint', 'ESLint');
runCommand('npm run test:coverage', 'Unit tests');
checkCoverage();
runCommand('npm run build', 'Build check');

console.log('🎉 All pre-commit checks passed!');
```

### Coverage Configuration
```javascript
// vitest.config.js
export default {
  test: {
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        'coverage/**',
        'dist/**',
        'packages/*/test{,s}/**',
        '**/*.d.ts',
        '**/*.test.{ts,js}',
        '**/*.test-d.ts',
        'test{,s}/**',
        'test{,-*}.{ts,js}',
        '**/*{.,-}test.{ts,js}',
        '**/__tests__/**',
        '**/{karma,rollup,webpack,vite,vitest,jest,ava,babel,nyc,cypress}.config.*',
        '**/.{eslint,mocha,prettier}rc.{js,cjs,yml}',
      ],
      thresholds: {
        global: {
          branches: 85,
          functions: 90,
          lines: 90,
          statements: 90,
        },
        // Per-file thresholds
        './src/lib/utils/**/*.js': {
          branches: 100,
          functions: 100,
          lines: 100,
          statements: 100,
        },
      },
    },
  },
};
```

### Performance Monitoring
```javascript
// scripts/performance-check.js
const fs = require('fs');
const path = require('path');

function analyzeBundleSize() {
  const buildPath = './dist';
  if (!fs.existsSync(buildPath)) {
    console.error('❌ Build directory not found');
    process.exit(1);
  }
  
  const files = fs.readdirSync(buildPath, { recursive: true })
    .filter(file => file.endsWith('.js') || file.endsWith('.css'))
    .map(file => {
      const filePath = path.join(buildPath, file);
      const stats = fs.statSync(filePath);
      return {
        name: file,
        size: stats.size,
        sizeKB: Math.round(stats.size / 1024),
      };
    });
  
  const totalSize = files.reduce((acc, file) => acc + file.size, 0);
  const totalSizeKB = Math.round(totalSize / 1024);
  
  console.log('📦 Bundle Analysis:');
  files.forEach(file => {
    console.log(`  ${file.name}: ${file.sizeKB} KB`);
  });
  console.log(`  Total: ${totalSizeKB} KB`);
  
  // Alert if bundle is too large
  const maxSizeKB = 500; // 500KB limit
  if (totalSizeKB > maxSizeKB) {
    console.error(`❌ Bundle size ${totalSizeKB} KB exceeds limit ${maxSizeKB} KB`);
    process.exit(1);
  }
  
  console.log('✅ Bundle size within limits');
}

analyzeBundleSize();
```

### Automated Accessibility Testing
```javascript
// tests/accessibility.test.js
import { test, expect } from '@playwright/test';
import AxeBuilder from '@axe-core/playwright';

test.describe('Accessibility tests', () => {
  test('main page should not have accessibility violations', async ({ page }) => {
    await page.goto('/');
    
    const accessibilityScanResults = await new AxeBuilder({ page }).analyze();
    
    expect(accessibilityScanResults.violations).toEqual([]);
  });

  test('symbol chooser should be keyboard accessible', async ({ page }) => {
    await page.goto('/');
    
    // Navigate using keyboard only
    await page.keyboard.press('Tab'); // Focus first element
    await page.keyboard.press('Tab'); // Focus symbol chooser button
    await page.keyboard.press('Enter'); // Open symbol chooser
    
    await expect(page.locator('[data-testid="symbol-chooser"]')).toBeVisible();
    
    // Scan for accessibility issues
    const accessibilityScanResults = await new AxeBuilder({ page })
      .include('[data-testid="symbol-chooser"]')
      .analyze();
    
    expect(accessibilityScanResults.violations).toEqual([]);
  });
});
```

## Advanced Testing & Debugging Patterns

### Visual Regression Testing
```javascript
// tests/visual.test.js
import { test, expect } from '@playwright/test';

test.describe('Visual regression tests', () => {
  test('sign editor maintains visual consistency', async ({ page }) => {
    await page.goto('/');
    
    // Set up consistent test environment
    await page.setViewportSize({ width: 1280, height: 720 });
    await page.waitForSelector('[data-testid="sign-editor"]');
    
    // Load test data
    await page.evaluate(() => {
      window.debugStores?.signData.set({
        name: 'Test Sign',
        symbols: [
          { id: 'symbol1', code: 'S10000', x: 100, y: 100 },
          { id: 'symbol2', code: 'S20000', x: 200, y: 150 }
        ]
      });
    });
    
    // Take screenshot
    await expect(page.locator('[data-testid="sign-editor"]')).toHaveScreenshot('sign-editor-basic.png');
  });

  test('symbol chooser displays correctly across themes', async ({ page }) => {
    const themes = ['light', 'dark', 'high-contrast'];
    
    for (const theme of themes) {
      await page.goto(`/?theme=${theme}`);
      await page.click('[data-testid="open-symbol-chooser"]');
      await page.waitForSelector('[data-testid="symbol-chooser"]');
      
      await expect(page.locator('[data-testid="symbol-chooser"]'))
        .toHaveScreenshot(`symbol-chooser-${theme}.png`);
    }
  });
});
```

### Memory Leak Detection
```javascript
// tests/memory.test.js
import { test, expect } from '@playwright/test';

test.describe('Memory leak tests', () => {
  test('no memory leaks in component lifecycle', async ({ page }) => {
    await page.goto('/');
    
    // Get initial memory baseline
    const initialMemory = await page.evaluate(() => performance.memory?.usedJSHeapSize || 0);
    
    // Perform operations that could cause leaks
    for (let i = 0; i < 10; i++) {
      await page.click('[data-testid="add-symbol"]');
      await page.click('[data-testid="symbol-S10000"]');
      await page.click('[data-testid="delete-symbol"]');
    }
    
    // Force garbage collection
    await page.evaluate(() => {
      if (window.gc) window.gc();
    });
    
    await page.waitForTimeout(1000);
    
    const finalMemory = await page.evaluate(() => performance.memory?.usedJSHeapSize || 0);
    const memoryIncrease = finalMemory - initialMemory;
    
    // Memory increase should be reasonable (less than 5MB)
    expect(memoryIncrease).toBeLessThan(5 * 1024 * 1024);
  });
});
```

### Advanced Mock Patterns
```typescript
// test-utils/advanced-mocks.ts
import { vi } from 'vitest';

// Mock with state persistence across tests
export function createPersistentMock<T extends (...args: any[]) => any>(
  implementation: T,
  persistKey: string
): T {
  const key = `mock_${persistKey}`;
  
  if (!globalThis[key]) {
    globalThis[key] = {
      callHistory: [],
      returnValues: [],
    };
  }
  
  return vi.fn((...args: Parameters<T>) => {
    const state = globalThis[key];
    state.callHistory.push(args);
    
    const result = implementation(...args);
    state.returnValues.push(result);
    
    return result;
  }) as T;
}

// Conditional mocking based on test environment
export function createConditionalMock<T>(
  realImplementation: T,
  mockImplementation: T,
  condition: () => boolean
): T {
  return ((...args: any[]) => {
    if (condition()) {
      return mockImplementation(...args);
    }
    return realImplementation(...args);
  }) as T;
}

// Time-based mocking for testing time-dependent code
export function createTimeMock() {
  const originalDate = Date;
  const originalNow = Date.now;
  const originalGetTime = Date.prototype.getTime;
  
  let mockTime = new Date('2025-01-01T00:00:00.000Z').getTime();
  
  return {
    setTime: (time: number | Date) => {
      mockTime = typeof time === 'number' ? time : time.getTime();
    },
    
    advance: (ms: number) => {
      mockTime += ms;
    },
    
    install: () => {
      globalThis.Date = class extends originalDate {
        constructor(...args: any[]) {
          if (args.length === 0) {
            super(mockTime);
          } else {
            super(...args);
          }
        }
        
        static now() {
          return mockTime;
        }
      } as DateConstructor;
      
      globalThis.Date.now = () => mockTime;
    },
    
    uninstall: () => {
      globalThis.Date = originalDate;
    }
  };
}

// Usage examples
describe('Advanced mocking patterns', () => {
  const timeMock = createTimeMock();
  
  beforeEach(() => {
    timeMock.install();
    timeMock.setTime('2025-01-01T00:00:00.000Z');
  });
  
  afterEach(() => {
    timeMock.uninstall();
  });
  
  test('time-dependent functionality', () => {
    const start = Date.now();
    timeMock.advance(1000);
    const end = Date.now();
    
    expect(end - start).toBe(1000);
  });
});
```

### Test Data Generation & Fuzzing
```typescript
// test-utils/generators.ts
import { faker } from '@faker-js/faker';

export interface SignDataGenerator {
  name?: string;
  symbolCount?: number;
  minX?: number;
  maxX?: number;
  minY?: number;
  maxY?: number;
}

export function generateSignData(options: SignDataGenerator = {}) {
  const {
    name = faker.lorem.words(2),
    symbolCount = faker.number.int({ min: 1, max: 10 }),
    minX = 0,
    maxX = 500,
    minY = 0,
    maxY = 400
  } = options;
  
  return {
    name,
    symbols: Array.from({ length: symbolCount }, (_, i) => ({
      id: `symbol-${i}-${faker.string.uuid()}`,
      code: `S${faker.number.int({ min: 10000, max: 99999 })}`,
      x: faker.number.int({ min: minX, max: maxX }),
      y: faker.number.int({ min: minY, max: maxY }),
      rotation: faker.number.int({ min: 0, max: 360 }),
      scale: faker.number.float({ min: 0.5, max: 2.0 }),
      fill: faker.color.rgb(),
    }))
  };
}

// Fuzzing tests for robust validation
export function createFuzzTest<T>(
  generator: () => T,
  validator: (input: T) => boolean,
  iterations = 1000
) {
  return () => {
    const failures: T[] = [];
    
    for (let i = 0; i < iterations; i++) {
      const input = generator();
      try {
        if (!validator(input)) {
          failures.push(input);
        }
      } catch (error) {
        failures.push(input);
      }
    }
    
    if (failures.length > 0) {
      console.log(`Fuzz test failures (${failures.length}/${iterations}):`, failures.slice(0, 5));
    }
    
    // Allow some failures but not too many
    expect(failures.length / iterations).toBeLessThan(0.01); // Less than 1% failure rate
  };
}

// Usage
describe('Fuzz testing', () => {
  test('sign validation handles random data', 
    createFuzzTest(
      generateSignData,
      (data) => validateSignData(data).isValid,
      500
    )
  );
});
```

### Performance Profiling in Tests
```javascript
// test-utils/profiler.js
export class TestProfiler {
  constructor() {
    this.marks = new Map();
    this.measures = new Map();
  }
  
  mark(name) {
    const markName = `${name}-${Date.now()}`;
    performance.mark(markName);
    this.marks.set(name, markName);
    return markName;
  }
  
  measure(name, startMark) {
    const endMark = this.mark(`${name}-end`);
    const measureName = `measure-${name}`;
    
    performance.measure(measureName, startMark || this.marks.get(name), endMark);
    
    const measure = performance.getEntriesByName(measureName)[0];
    this.measures.set(name, measure.duration);
    
    return measure.duration;
  }
  
  getReport() {
    return Object.fromEntries(this.measures);
  }
  
  assertPerformance(name, maxMs) {
    const duration = this.measures.get(name);
    if (duration > maxMs) {
      throw new Error(`Performance assertion failed: ${name} took ${duration}ms, expected < ${maxMs}ms`);
    }
  }
}

// Usage in tests
describe('Performance tests', () => {
  let profiler;
  
  beforeEach(() => {
    profiler = new TestProfiler();
  });
  
  test('sign validation performance', () => {
    const signData = generateSignData({ symbolCount: 100 });
    
    profiler.mark('validation-start');
    validateSignData(signData);
    profiler.measure('validation', 'validation-start');
    
    profiler.assertPerformance('validation', 10); // Should complete in under 10ms
  });
});
```
