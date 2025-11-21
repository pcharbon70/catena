# Type Error System Documentation

Welcome to the comprehensive documentation for the Catena type error formatting system.

## 📚 Documentation Overview

This system provides excellent developer experience for type errors with:
- 🎨 **Visual highlighting** of type differences
- 📚 **Educational explanations** that help developers learn
- 🔧 **Actionable fix suggestions** with code examples
- 🎛️ **Configurable verbosity** for different use cases
- 🔍 **Human-readable type names** using Greek letters (alpha, beta, gamma, etc.)

## 🚀 Quick Start

Looking for the fastest way to get started? Check out our:

- **[Cheat Sheet](TYPE_ERROR_CHEAT_SHEET.md)** - Quick reference for common patterns
- **[Developer Guide](TYPE_ERROR_DEVELOPER_GUIDE.md)** - Comprehensive integration guide

## 📖 Core Modules

### Enhanced Error Formatter
**`src/compiler/types/catena_type_error_formatter.erl`**

- Rich type error formatting with visual highlighting
- Three verbosity levels: `terse`, `normal`, `verbose`
- Context-aware error messages
- Perfect for integration with compilers and IDEs

### Educational Error Explanations  
**`src/compiler/types/catena_type_error_explain.erl`**

- Pattern-matched explanations for common mistakes
- Step-by-step fix suggestions with code examples
- Context-aware help for learning developers
- 20 comprehensive test cases covering all functionality

### Human-Readable Type Pretty-Printing
**`src/compiler/types/catena_type_pp.erl`**

- Smart fresh variable generation (alpha, beta, gamma...)
- Reserved keyword handling (maybe_type → maybe)
- Clean, readable type output for developers

## 🎯 Key Features

### Visual Examples

**Terse Mode (IDE tooltips):**
```
Expected integer, got string
```

**Normal Mode (Compiler output):**
```
Type mismatch:
  Expected: integer
  Got:      string

These types are incompatible:
  - Expected type 'integer'
  - Got type 'string'
```

**Verbose Mode (Learning environment):**
```
Type mismatch...

=== Detailed Analysis ===
Type categories:
  Expected: Basic type 'integer'
  Actual:    Basic type 'string'

Suggestions:
  - Check the type signatures of functions you're calling
  - Consider using explicit type annotations
  - Look for missing or extra function calls
```

### Supported Error Types

- ✅ **Type mismatches** - All type categories with specialized explanations
- ✅ **Function vs value confusion** - Common beginner mistakes  
- ✅ **Container type issues** - List/maybe confusion, element problems
- ✅ **Record field problems** - Missing/extra fields, type mismatches
- ✅ **Tuple size errors** - Wrong number of elements
- ✅ **Variant type conflicts** - Incompatible sum types
- ✅ **Missing trait instances** - Educational constraint messages
- ✅ **Arity mismatches** - Wrong function argument counts
- ✅ **Infinite type attempts** - Occurs check failures
- ✅ **Unbound variables** - Scope and naming issues
- ✅ **Ambiguous types** - Type resolution problems

## 🔧 Integration Examples

### Basic Integration
```erlang
% Simple error formatting
Error = {unification_error, {tcon, integer}, {tcon, string}},
Result = catena_type_error_formatter:format_error(Error, #{}),
% -> Human-readable error message
```

### Complete Integration
```erlang
% Combine formatting with educational content
Formatted = catena_type_error_formatter:format_error(Error, Context),
Explanation = catena_type_error_explain:explain_error(Error, Context),
Fixes = catena_type_error_explain:suggest_fix(Error, Context),
Complete = [Formatted, "\n\n", Explanation, "\n\n", Fixes]
```

### IDE Integration
```erlang
% Space-efficient tooltips
format_type_mismatch(Expected, Actual, Context, #{
  verbosity => terse
})
% -> "Expected integer, got string"
```

## 🧪 Testing

Comprehensive test coverage ensures reliability:

- **20 test cases** for error explanations and fix suggestions
- **14 test cases** for error formatting and highlighting
- **Zero test failures** across all modules
- **100% API coverage** for exported functions

Run tests:
```bash
eunit:test(catena_type_error_formatter_tests)
eunit:test(catena_type_error_explain_tests)
```

## 🎨 Design Principles

- **Educational Focus** - Help developers learn, not just report errors
- **Actionable Advice** - Specific fixes, not generic messages
- **Context Awareness** - Adapt messages to error context
- **Visual Clarity** - Highlight differences and structure
- **Developer Experience** - Human-readable output over technical precision
- **Configurable Detail** - Appropriate verbosity for different contexts

## 📋 Implementation Status

- ✅ **Priority 1**: Type variable system modernization with human-readable names
- ✅ **Priority 2**: Verbosity system implemented (terse/normal/verbose)
- ✅ **Priority 3**: Consistent atom naming (maybe_type → maybe output)
- ✅ **Priority 4**: Comprehensive test coverage (340 lines, 20 tests)
- ✅ **Priority 5**: Enhanced documentation and developer guides

## 🔍 Deep Dives

- **[Type Variable Names](TYPE_ERROR_DEVELOPER_GUIDE.md#type-variables)** - Fresh variable generation
- **[Verbosity Levels](TYPE_ERROR_DEVELOPER_GUIDE.md#verbosity-options)** - When to use each mode
- **[Integration Patterns](TYPE_ERROR_DEVELOPER_GUIDE.md#integration-patterns)** - Best practices
- **[Error Type Reference](TYPE_ERROR_DEVELOPER_GUIDE.md#error-type-reference)** - Complete error catalog
- **[Extending the System](TYPE_ERROR_DEVELOPER_GUIDE.md#extending-the-system)** - Adding new features

---

## 💡 Quick Tips

1. **Always provide context** when available for better error messages
2. **Use terse mode** for space-constrained UI (tooltips, status bars)
3. **Use verbose mode** for learning environments and debugging
4. **Combine with explanations** for comprehensive developer support
5. **Test with real data** to validate integration patterns

🎉 **Ready to provide excellent type error experiences!**

Start with the [Cheat Sheet](TYPE_ERROR_CHEAT_SHEET.md) for quick reference, or dive into the [Developer Guide](TYPE_ERROR_DEVELOPER_GUIDE.md) for comprehensive integration details.
