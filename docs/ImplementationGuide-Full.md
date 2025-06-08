# QuickSignEditor 2025 - Svelte 3 Implementation Guide

## Development Process Overview

### AI-Optimized Workflow
- **Small Functions**: ≤20 lines for better GitHub Copilot suggestions
- **Clear Names**: Intention-revealing function and variable names  
- **TypeScript**: Use TypeScript for better AI understanding and type safety
- **Pure Functions First**: Business logic without side effects
- **Component Isolation**: Test components in isolation before integration

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
```

## Development Rules

### TDD Cycle
1. **RED**: Write failing test
2. **GREEN**: Minimal code to pass
3. **REFACTOR**: Improve while tests stay green
4. **REPEAT**: Small iterations

### Quality Gates
- All tests pass before new development
- Test coverage ≥ 90% for business logic
- No broken tests committed
- Functions stay under 20 lines

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

## Testing Strategy

### Tier 1: Pure Functions (100% coverage required)
```javascript
// validation.test.js
import { describe, it, expect } from 'vitest';
import { validateSignData } from '../lib/utils/validation.js';

describe('validateSignData', () => {
  it('returns valid for complete data', () => {
    const result = validateSignData({ name: 'test', symbols: ['symbol1'] });
    expect(result.isValid).toBe(true);
  });
  
  it('returns invalid for missing name', () => {
    const result = validateSignData({ symbols: ['symbol1'] });
    expect(result.isValid).toBe(false);
    expect(result.error).toBe('Name required');
  });
});
```

### Tier 2: Component Testing (95% coverage required)
```javascript
// SignEditor.test.js
import { render, screen } from '@testing-library/svelte';
import SignEditor from '../lib/components/SignEditor.svelte';

test('displays error for invalid data', () => {
  render(SignEditor, { signData: {} });
  expect(screen.getByText('Name required')).toBeInTheDocument();
});
```

### Tier 3: Integration Testing (90% coverage required)
```javascript
// integration.test.js
import { test, expect } from '@playwright/test';

test('complete sign creation workflow', async ({ page }) => {
  await page.goto('/');
  await page.fill('[data-testid="sign-name"]', 'My Sign');
  await page.click('[data-testid="add-symbol"]');
  await page.click('[data-testid="save-sign"]');
  
  await expect(page.locator('[data-testid="success-message"]')).toBeVisible();
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

### Tier 3 Complete
- [ ] Full user workflows implemented
- [ ] E2E tests covering main user journeys
- [ ] 90% overall test coverage
- [ ] Performance validated

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
 

This approach ensures:
- Faster debugging (catch issues immediately)
- Better code quality (small, focused changes)
- Reduced integration problems
- Confidence in refactoring
- Maintainable codebase

### 1.4 Progressive Development Methodology

**⚠️ CRITICAL: Follow this three-tier development approach for all code changes:**

#### Tier 1: Pure Function Development (Foundation Layer)
**Characteristics:**
- No side effects (database calls, file I/O, network requests)
- Deterministic output for same input
- Easy to test and reason about
- AI-friendly (predictable patterns)

**Development Process:**
1. **Design Pure Functions**: Identify core business logic that can be pure
2. **Write Function Signatures**: Define clear input/output types
3. **Create Tests**: Write comprehensive unit tests
4. **Implement**: Write minimal code to pass tests
5. **Refactor**: Improve without breaking tests

**Example Structure:**
```csharp
// Pure function example
public static Result<ValidatedData> ValidateSignData(RawSignData input)
{
    // No side effects - only transforms data
    return input.IsValid() 
        ? Result.Success(new ValidatedData(input))
        : Result.Failure("Invalid sign data");
}
```

#### Tier 2: Integration Development (Service Layer)
**Characteristics:**
- Orchestrates pure functions with external systems
- Handles database operations, API calls, file I/O
- Uses dependency injection for testability
- Composed of pure functions + side effects

**Development Process:**
1. **Compose Pure Functions**: Combine tested pure functions
2. **Add Integration Points**: Database, API, file system interactions
3. **Write Integration Tests**: Mock external dependencies
4. **Test Error Scenarios**: Network failures, database errors
5. **Validate with Real Systems**: Test against actual dependencies

#### Tier 3: End-to-End Development (Application Layer)
**Characteristics:**
- Full user workflows and scenarios
- System-level integration testing
- Performance and security validation
- Complete feature functionality

**Development Process:**
1. **Define User Scenarios**: Complete workflows from start to finish
2. **Write E2E Tests**: Test entire feature pipelines
3. **Implement Coordination Logic**: Tie together all layers
4. **Performance Testing**: Validate under realistic conditions
5. **Security Testing**: Verify secure data handling

#### Functional Programming Guidelines

**✅ Preferred Patterns:**
- Pure functions for business logic
- Immutable data structures where possible
- Result/Option types for error handling
- Function composition over inheritance
- Explicit over implicit operations

**❌ Avoid These Patterns:**
- Deep function currying (hard for AI to understand)
- Long method chains (difficult to debug)
- Complex lambda expressions (prefer named functions)
- Heavy use of higher-order functions
- Implicit type conversions

**AI-Friendly Function Design:**
```csharp
// Good: Clear, testable, AI-readable
public static ValidationResult ValidateEmail(string email)
{
    if (string.IsNullOrWhiteSpace(email))
        return ValidationResult.Invalid("Email cannot be empty");
    
    if (!email.Contains("@"))
        return ValidationResult.Invalid("Email must contain @");
    
    return ValidationResult.Valid();
}

// Avoid: Complex chaining
public static bool IsValidEmail(string email) =>
    Optional.Of(email)
        .Filter(e => !string.IsNullOrWhiteSpace(e))
        .Map(e => e.Trim().ToLower())
        .Filter(e => e.Contains("@"))
        .Map(e => Regex.IsMatch(e, EmailPattern))
        .GetValueOrDefault(false);
```

#### Test-First Development Cycle
1. **Start with Green Tests**: Always ensure all existing tests pass before making any changes
2. **Write Tests First**: Create tests for the new functionality before implementing
3. **Implement Incrementally**: Make small, focused changes
4. **Test Immediately**: Run tests after each small change
5. **Iterate Until Green**: Fix issues immediately before moving forward
6. **Run Full Test Suite**: Verify all tests pass before moving to next feature

#### Progressive Development Steps
  
#### Development Rules
- **Never commit broken tests**
- **Never skip running tests after changes**
- **Fix broken tests immediately - don't accumulate technical debt**
- **One failing test at a time - fix before adding more**
- **Small iterations - prefer 10 small changes over 1 large change**
- **Test coverage must not decrease with new code**

#### When Things Go Wrong
If tests fail:
1. **Stop all new development**
2. **Identify the root cause**
3. **Fix the issue immediately**
4. **Verify the fix with tests**
5. **Only then continue with new features**

This approach ensures:
- ✅ Faster debugging (issues are caught immediately)
- ✅ Better code quality (small, focused changes)
- ✅ Reduced integration problems
- ✅ Confidence in refactoring
- ✅ Maintainable codebase

## 2. Development Phases

### Phase 1: Project Foundation (Week 1)

#### 2.1 Project Setup
 
#### 2.2 Package Installation
  
## 5. Comprehensive Testing Strategy

### 5.1 Testing Overview

#### 5.1.1 Progressive Testing Philosophy

**⚠️ MANDATORY: All AI code generation and modifications must follow this testing approach:**

##### Core Testing Principles
- **Green Before Red**: Always start with all tests passing before making changes
- **Red-Green-Refactor**: Write failing test → Make it pass → Improve code quality
- **Incremental Progress**: Work in small iterations with immediate test feedback
- **Test-Driven**: Tests guide implementation, not the other way around
- **Fail Fast**: Stop and fix immediately when tests break

#### Testing Workflow for AI Code Generation

**AI-Optimized Development Sequence:**

1. **Tier 1: Pure Function Development**
   - Write function signature with clear types
   - Create comprehensive unit tests
   - Implement minimal working code
   - Refactor for clarity and performance
   - Validate 100% test coverage for pure functions

2. **Tier 2: Integration Development**
   - Compose pure functions into service operations
   - Add integration tests with mocked dependencies
   - Implement database/API integration code
   - Test error scenarios and edge cases
   - Validate integration test coverage >95%

3. **Tier 3: End-to-End Development**
   - Design complete user workflows
   - Write comprehensive E2E tests
   - Implement coordination and orchestration logic
   - Performance and security testing
   - Validate full system integration

**GitHub Copilot Optimization Tips:**
- **Small Functions**: Keep functions under 20 lines for better AI suggestions
- **Clear Naming**: Use descriptive names that explain intent
- **Type Safety**: Explicit types help AI understand context
- **Comment Context**: Brief comments explaining business rules
- **Consistent Patterns**: Follow established patterns within the codebase

**Development Iteration Cycle:**

1. **Phase 1: Pure Function Test Creation**
   ```csharp
   [Test]
   public void ValidateSignData_WithValidInput_ReturnsSuccess()
   {
       // Arrange
       var validInput = new RawSignData { /* valid data */ };
       
       // Act
       var result = SignValidator.ValidateSignData(validInput);
       
       // Assert
       Assert.That(result.IsSuccess, Is.True);
   }
   ```

2. **Phase 2: Pure Function Implementation**
   ```csharp
   public static Result<ValidatedSignData> ValidateSignData(RawSignData input)
   {
       // Implementation focuses only on data transformation
       // No side effects, fully testable
       return input.IsValid() 
           ? Result.Success(new ValidatedSignData(input))
           : Result.Failure("Validation failed");
   }
   ```

3. **Phase 3: Integration Layer Testing**
   ```csharp
   [Test]
   public async Task SaveSignData_WithValidData_CallsRepository()
   {
       // Test integration with mocked dependencies
       var mockRepository = new Mock<ISignRepository>();
       var service = new SignService(mockRepository.Object);
       
       await service.SaveSignData(validData);
       
       mockRepository.Verify(r => r.Save(It.IsAny<ValidatedSignData>()), Times.Once);
   }
   ```

4. **Phase 4: End-to-End Testing**
   ```csharp
   [Test]
   public async Task CompleteSignWorkflow_FromInputToStorage_WorksCorrectly()
   {
       // Arrange
       var inputFile = "test-signs.json";
       var expectedSignCount = 10;

       // Act
       var result = await signWorkflow.ProcessSignFromInput(rawInput);
       
       // Assert
       Assert.That(result.IsSuccess, Is.True);
       Assert.That(await repository.ExistsAsync(result.Value.Id), Is.True);
   }
   ```
  

5. **Repeat**: Only move to next feature after all tests are green

##### Documentation and Coverage Iteration

**AI-Optimized Documentation Workflow:**

- **After Each Tier**: Update documentation to reflect completed functionality
- **Code Coverage by Tier**: 
  - Tier 1 (Pure Functions): 100% coverage required
  - Tier 2 (Integration): 95% coverage required  
  - Tier 3 (End-to-End): 90% coverage required
- **Living Documentation**: Keep README and implementation guides current
- **AI Context Updates**: Ensure documentation provides clear context for AI tools

**Iteration Process:**
1. **Complete Pure Functions**: Implement and test all pure functions for a feature
2. **Update Function Documentation**: Document pure function interfaces and contracts
3. **Verify Pure Function Coverage**: Ensure 100% test coverage before proceeding
4. **Complete Integration Layer**: Add service layer with external dependencies
5. **Update Integration Documentation**: Document service contracts and dependencies
6. **Verify Integration Coverage**: Ensure 95% test coverage before proceeding
7. **Complete End-to-End Layer**: Implement full user workflows
8. **Update Workflow Documentation**: Document complete user scenarios
9. **Verify E2E Coverage**: Ensure 90% test coverage before feature completion
10. **Update All Affected Documentation**: README, API docs, troubleshooting guides

**Documentation Standards for AI Agents:**
- **Clear Function Signatures**: Include parameter descriptions and return types
- **Business Context**: Explain the 'why' behind complex business rules
- **Error Scenarios**: Document expected error conditions and handling
- **Dependencies**: Clearly list all external dependencies and their purposes
- **Examples**: Provide concrete usage examples for complex operations

##### Testing Error Recovery

**When Tests Fail During Development:**
1. **STOP** - Do not add more code
2. **ANALYZE** - Identify exact failure cause
3. **FIX** - Address the specific issue
4. **VERIFY** - Run tests to confirm fix
5. **CONTINUE** - Only then proceed with development

**Escalation Path:**
- 1-2 failures: Debug and fix immediately
- 3+ failures: Review implementation approach
- Persistent failures: Rollback changes and restart with smaller increments

#### 5.1.2 Testing Objectives
- **Functional Correctness**: Verify all business requirements are met
- **Data Integrity**: Ensure accurate data transformation and storage
- **Error Handling**: Validate graceful handling of failure scenarios
- **Performance**: Confirm system meets performance requirements
- **Security**: Validate secure handling of credentials and data

#### 5.1.3 Testing Scope
- ✅ Unit tests for all business logic (target: >90% coverage)
- ✅ Integration tests for API and database operations
- ✅ Contract tests for API response validation
- ✅ Performance tests for data processing scenarios
- ✅ Security tests for authentication and data handling
- ✅ Recurring synchronization and scheduling tests
- ❌ UI tests (not applicable for console application)
- ❌ Load tests beyond single-user scenarios

### 5.2 Test Strategy by Layer

#### 5.2.1 Pure Function Layer Testing (Tier 1)
**Focus**: Business logic without side effects

**Testing Approach:**
- **100% Coverage Required**: Pure functions must be completely tested
- **Property-Based Testing**: Use random inputs to verify function properties
- **Edge Case Testing**: Boundary conditions, null inputs, empty collections
- **Performance Testing**: Validate algorithmic efficiency

**Example Test Structure:**
```csharp
[TestFixture]
public class SignValidationTests
{
    [Test]
    [TestCase("valid-sign", ExpectedResult = true)]
    [TestCase("", ExpectedResult = false)]
    [TestCase(null, ExpectedResult = false)]
    public bool ValidateSignFormat_VariousInputs_ReturnsExpectedResult(string input)
    {
        return SignValidator.IsValidFormat(input).IsSuccess;
    }

    [Test]
    public void TransformSignData_WithComplexInput_PreservesIntegrity()
    {
        // Arrange
        var complexInput = CreateComplexSignData();
        
        // Act
        var result = SignTransformer.Transform(complexInput);
        
        // Assert
        Assert.That(result.Checksum, Is.EqualTo(complexInput.CalculateChecksum()));
    }
}
```

#### 5.2.2 Integration Layer Testing (Tier 2)
**Focus**: Service coordination and external system interaction

**Testing Approach:**
- **Mock External Dependencies**: Database, APIs, file systems
- **Test Error Scenarios**: Network failures, timeouts, invalid responses
- **Verify Integration Contracts**: Ensure proper data flow between systems
- **Transaction Testing**: Verify rollback and consistency

**Example Test Structure:**
```csharp
[TestFixture]
public class SignServiceIntegrationTests
{
    private Mock<ISignRepository> _mockRepository;
    private Mock<IApiClient> _mockApiClient;
    private SignService _signService;

    [SetUp]
    public void SetUp()
    {
        _mockRepository = new Mock<ISignRepository>();
        _mockApiClient = new Mock<IApiClient>();
        _signService = new SignService(_mockRepository.Object, _mockApiClient.Object);
    }

    [Test]
    public async Task ProcessSign_WithValidData_CallsAllDependencies()
    {
        // Arrange
        var signData = CreateValidSignData();
        _mockApiClient.Setup(c => c.ValidateAsync(It.IsAny<SignData>()))
                     .ReturnsAsync(ApiResponse.Success());

        // Act
        var result = await _signService.ProcessSignAsync(signData);

        // Assert
        Assert.That(result.IsSuccess, Is.True);
        _mockRepository.Verify(r => r.SaveAsync(It.IsAny<ProcessedSign>()), Times.Once);
        _mockApiClient.Verify(c => c.ValidateAsync(signData), Times.Once);
    }
}
```

#### 5.2.3 End-to-End Layer Testing (Tier 3)
**Focus**: Complete user workflows and system integration

**Testing Approach:**
- **Real System Integration**: Test against actual databases and APIs
- **User Journey Testing**: Complete workflows from input to output
- **Performance Under Load**: Validate system behavior with realistic data
- **Security Integration**: Test authentication and authorization flows

**Example Test Structure:**
```csharp
[TestFixture]
public class SignWorkflowEndToEndTests
{
    private TestContainer _testContainer;
    private ISignWorkflow _workflow;

    [OneTimeSetUp]
    public void OneTimeSetUp()
    {
        _testContainer = new TestContainer();
        _workflow = _testContainer.Resolve<ISignWorkflow>();
    }

    [Test]
    public async Task CompleteSignProcessing_FromFileToDatabase_WorksCorrectly()
    {
        // Arrange
        var inputFile = "test-signs.json";
        var expectedSignCount = 10;

        // Act
        var result = await _workflow.ProcessSignFileAsync(inputFile);

        // Assert
        Assert.That(result.ProcessedCount, Is.EqualTo(expectedSignCount));
        
        // Verify data actually exists in database
        var savedSigns = await _testContainer.Repository.GetAllAsync();
        Assert.That(savedSigns.Count(), Is.EqualTo(expectedSignCount));
    }
}
```

##### 5.2.3.1 Database Integration Tests 
##### 5.2.3.2 API Integration Tests 

#### 5.2.4 End-to-End Testing
 
### 5.4 Performance Testing
 

### 5.5 Test Configuration and CI/CD
 

### 5.6 Code Coverage and Quality Gates

#### 5.6.1 Coverage Requirements by Development Tier
- **Tier 1 (Pure Functions)**: 100% line coverage (no exceptions)
- **Tier 2 (Integration Layer)**: 95% line coverage  
- **Tier 3 (End-to-End Layer)**: 90% line coverage
- **Overall Project Coverage**: Minimum 95%

**AI Agent Optimization Guidelines:**
- **Function Size**: Keep functions under 20 lines for better AI analysis
- **Clear Contracts**: Explicit input/output types improve AI suggestions
- **Consistent Patterns**: Follow established patterns for predictable AI behavior
- **Meaningful Names**: Use intention-revealing names for better AI context understanding

**GitHub Copilot Best Practices:**
- **Context Files**: Keep related functions in the same file for better AI context
- **Type Annotations**: Always include explicit types for better AI understanding
- **Comment Standards**: Use // for single-line explanations, /// for API documentation
- **Pattern Consistency**: Maintain consistent error handling and validation patterns
 
#### 6.2 Documentation Completion
- README with setup instructions
- API documentation
- Deployment guide
- Troubleshooting guide

## 3. Implementation Checkpoints

### 3.1 Tier 1 Completion Checklist (Pure Functions)
- [ ] All core business logic identified and extracted into pure functions
- [ ] Function signatures designed with explicit types
- [ ] Comprehensive unit tests written and passing
- [ ] 100% test coverage achieved for all pure functions
- [ ] Functions are under 20 lines each for AI optimization
- [ ] No side effects in any pure function
- [ ] All pure functions are deterministic
- [ ] Clear, intention-revealing function names
- [ ] Documentation updated for pure function interfaces

### 3.2 Tier 2 Completion Checklist (Integration Layer)
- [ ] Pure functions composed into service operations
- [ ] External dependencies properly injected
- [ ] Integration tests written with mocked dependencies
- [ ] Error scenarios tested (network failures, timeouts, etc.)
- [ ] 95% test coverage achieved for integration layer
- [ ] Service contracts well-defined and documented
- [ ] Transaction boundaries clearly established
- [ ] Rollback and error recovery mechanisms tested
- [ ] Integration documentation updated

### 3.3 Tier 3 Completion Checklist (End-to-End)
- [ ] Complete user workflows implemented and tested
- [ ] End-to-end tests using real dependencies
- [ ] Performance requirements validated
- [ ] Security requirements tested
- [ ] 90% test coverage achieved for E2E layer
- [ ] User scenario documentation complete
- [ ] Troubleshooting guide updated
- [ ] Deployment procedures validated
- [ ] Monitoring and logging implemented

### 3.4 AI Agent Optimization Checklist
- [ ] Functions kept under 20 lines for better AI suggestions
- [ ] Consistent naming patterns across codebase
- [ ] Clear type annotations for AI context
- [ ] Meaningful comments explaining business logic
- [ ] Related functions grouped in same files
- [ ] Predictable error handling patterns
- [ ] GitHub Copilot suggestions actively utilized
- [ ] Code patterns consistent throughout project

### 3.5 Quality Gates
- [ ] All tests pass across all tiers
- [ ] Overall test coverage ≥ 95%
- [ ] No broken tests committed to repository
- [ ] Documentation reflects current implementation
- [ ] Performance benchmarks met
- [ ] Security requirements validated
- [ ] Code review completed for each tier
- [ ] AI-assisted code review checklist completed

## 4. AI-Optimized Code Organization

### 4.1 File Structure for AI Agents

**Pure Function Modules:**
```
src/
├── Core/
│   ├── Pure/
│   │   ├── SignValidation.cs      // Pure validation functions
│   │   ├── DataTransformation.cs  // Pure data transformation
│   │   ├── BusinessRules.cs       // Pure business logic
│   │   └── Calculations.cs        // Pure mathematical operations
│   ├── Types/
│   │   ├── SignData.cs           // Immutable data types
│   │   ├── ValidationResult.cs    // Result types
│   │   └── Errors.cs             // Error types
│   └── Contracts/
│       ├── ISignRepository.cs     // Interface definitions
│       └── ISignService.cs        // Service contracts
├── Services/
│   ├── SignService.cs            // Integration layer
│   ├── DataService.cs            // Data access coordination
│   └── WorkflowService.cs        // End-to-end workflows
└── Infrastructure/
    ├── Database/
    ├── External/
    └── Configuration/
```

### 4.2 GitHub Copilot Optimization Strategies

#### 4.2.1 Context-Aware File Organization
- **Group Related Functions**: Keep related pure functions in the same file
- **Consistent Naming**: Use predictable naming patterns across the codebase
- **Clear Separation**: Separate pure functions from integration code
- **Type Proximity**: Keep data types close to functions that use them

#### 4.2.2 AI-Friendly Code Patterns

**Preferred Function Structure:**
```csharp
/// <summary>
/// Validates sign data according to business rules
/// </summary>
/// <param name="signData">The sign data to validate</param>
/// <returns>Validation result with success/failure status</returns>
public static ValidationResult ValidateSignData(SignData signData)
{
    // Step 1: Null check
    if (signData == null)
        return ValidationResult.Failure("Sign data cannot be null");
    
    // Step 2: Required field validation
    if (string.IsNullOrWhiteSpace(signData.Name))
        return ValidationResult.Failure("Sign name is required");
    
    // Step 3: Business rule validation
    if (!IsValidSignFormat(signData.Format))
        return ValidationResult.Failure("Invalid sign format");
    
    return ValidationResult.Success();
}
```

**AI Context Hints:**
```csharp
// AI Context: This function transforms raw sign data into validated format
// Input: Raw sign data from external source
// Output: Validated sign data ready for storage
public static Result<ValidatedSignData> TransformSignData(RawSignData input)
{
    // Implementation follows transformation pattern
}
```

#### 4.2.3 Testing Patterns for AI Agents

**Consistent Test Structure:**
```csharp
[TestFixture]
public class SignValidationTests
{
    // AI Context: Test pure function with various inputs
    [Test]
    [TestCase("valid-sign", true)]
    [TestCase("", false)]
    [TestCase(null, false)]
    public void ValidateSignData_VariousInputs_ReturnsExpectedResult(string input, bool expected)
    {
        // Arrange
        var signData = new SignData { Name = input };
        
        // Act
        var result = SignValidator.ValidateSignData(signData);
        
        // Assert
        Assert.That(result.IsSuccess, Is.EqualTo(expected));
    }
}
```

### 4.3 Progressive Development Workflow

#### 4.3.1 Feature Development Sequence

**Step 1: Define Pure Functions**
1. Identify core business logic
2. Design function signatures with clear types
3. Write comprehensive tests
4. Implement functions with AI assistance
5. Validate 100% test coverage

**Step 2: Build Integration Layer**
1. Compose pure functions into services
2. Add external dependency interfaces
3. Write integration tests with mocks
4. Implement service layer
5. Validate 95% test coverage

**Step 3: Create End-to-End Workflows**
1. Design complete user scenarios
2. Write E2E tests with real dependencies
3. Implement workflow coordination
4. Performance and security testing
5. Validate 90% test coverage

#### 4.3.2 AI-Assisted Code Review Checklist

**Pure Function Review:**
- [ ] Function has no side effects
- [ ] Function is deterministic
- [ ] Function has clear input/output types
- [ ] Function is under 20 lines
- [ ] Function has descriptive name
- [ ] Function has comprehensive tests

**Integration Code Review:**
- [ ] External dependencies are injected
- [ ] Error handling is comprehensive
- [ ] Integration contracts are well-defined
- [ ] Mocked dependencies in tests
- [ ] Real integration tests exist

**End-to-End Review:**
- [ ] Complete user workflows tested
- [ ] Performance requirements met
- [ ] Security requirements validated
- [ ] Error scenarios covered
- [ ] Documentation updated
