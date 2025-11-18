# Catena Development Guides

This directory contains comprehensive guides for developing, testing, and maintaining the Catena compiler.

## Available Guides

### Testing and Quality Assurance

- **[Test Coverage Guide](test-coverage-guide.md)** - Complete guide to test coverage reporting
  - Running coverage analysis
  - Interpreting coverage results
  - Improving test coverage
  - Coverage targets and best practices

### Code Standards and Conventions

- **[Error Constructor Naming Convention](error-constructor-naming-convention.md)** - Standard pattern for error constructors
  - Consistent naming guidelines
  - Rationale and examples
  - Migration from old patterns

## Quick Reference

### Running Tests with Coverage

```bash
# Run all tests with coverage reporting
make coverage

# View coverage summary
make coverage-report

# Run tests without coverage
make test

# Clean build artifacts
make clean
```

### Coverage Results (Current)

Latest coverage results for type system modules:

| Module | Coverage | Status |
|--------|----------|--------|
| catena_type_scheme | 100% | ✅ Excellent |
| catena_type_env | 100% | ✅ Excellent |
| catena_types | 98% | ✅ Excellent |
| catena_type_subst | 96% | ✅ Excellent |
| catena_type_pp | 95% | ✅ Excellent |
| catena_type_error | 91% | ✅ Excellent |
| catena_type_state | 87% | ✅ Good |
| catena_compiler_utils | 40% | ⚠️ Moderate |

**Overall Type System Coverage: ~86%**

**Total Tests: 248 (all passing)**

### Next Steps to Improve Coverage

1. **catena_type_error**: 4 uncovered lines are in `format_error_with_location/2` which depends on `catena_location` module (not yet implemented)
2. **catena_compiler_utils**:
   - 95/235 lines covered (40%)
   - Remaining uncovered code includes:
     - Some `extract_location/1` and `format_location/1` cases that depend on `catena_location` module
     - Many AST node type-specific clauses in extract_location (40+ patterns)
     - Will improve when `catena_location` module is implemented (Task 1.2.4) and parser integration tests are added

## Contributing

When adding new features or fixing bugs:

1. Write tests first (TDD approach)
2. Run `make coverage` to verify tests execute new code
3. Aim for ≥90% coverage on modified modules
4. Include coverage stats in pull request descriptions

## Directory Structure

```
notes/guides/
├── README.md                                # This file
├── test-coverage-guide.md                   # Detailed coverage guide
└── error-constructor-naming-convention.md   # Error naming standards
```

Future guides to be added:
- Debugging guide
- Performance profiling guide
- Code review checklist
- Contributing guide
- Property-based testing guide
