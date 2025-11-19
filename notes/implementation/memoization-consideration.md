# Memoization for Type Formatting - Design Consideration

**Date:** November 19, 2024
**Module:** catena_type_error_formatter
**Status:** Deferred

## Overview

This document evaluates the potential benefits and implementation approach for adding memoization to the type formatting system.

## Current Performance Characteristics

The type formatter currently:
- Recursively traverses type structures
- Has depth limiting (MAX_DIFF_DEPTH = 10) to prevent stack overflow
- Formats each type component on demand
- No caching of formatted results

## When Memoization Would Help

Memoization would provide benefits when:
1. The same types are formatted multiple times (e.g., in REPL sessions)
2. Large recursive types with repeated substructures
3. Multiple errors involving the same types
4. Interactive development with frequent recompilation

## When Memoization Might Not Help

Memoization may not be beneficial when:
1. Types are mostly unique (typical compilation scenario)
2. Memory usage is a concern
3. Types contain location-specific information that changes

## Proposed Implementation Approach

If implemented, the memoization system would:

```erlang
%% ETS table for caching formatted types
-define(FORMAT_CACHE, catena_type_format_cache).

%% Initialize cache (call on startup)
init_format_cache() ->
    ets:new(?FORMAT_CACHE, [
        named_table,
        set,
        public,
        {read_concurrency, true}
    ]).

%% Format with caching
format_type_with_cache(Type, Opts) ->
    %% Create cache key from type and options
    Key = {Type, maps:get(verbosity, Opts, normal)},

    case ets:lookup(?FORMAT_CACHE, Key) of
        [{_, Formatted}] ->
            %% Cache hit
            Formatted;
        [] ->
            %% Cache miss - format and store
            Formatted = format_type_impl(Type, Opts),
            ets:insert(?FORMAT_CACHE, {Key, Formatted}),
            Formatted
    end.

%% Clear cache (e.g., on module reload)
clear_format_cache() ->
    ets:delete_all_objects(?FORMAT_CACHE).
```

## Performance Impact Analysis

### Without Memoization
- Time complexity: O(n) where n is the size of the type
- Space complexity: O(d) where d is the depth (stack space)
- No persistent memory usage

### With Memoization
- Time complexity: O(1) for cache hits, O(n) for misses
- Space complexity: O(n*m) where m is number of unique types cached
- Persistent memory usage grows with unique types

## Recommendation

**Current Recommendation:** DEFER

Reasons:
1. No performance issues reported with current implementation
2. Depth limiting already prevents worst-case scenarios
3. Most compilation scenarios won't benefit significantly
4. Adds complexity and memory overhead

**Future Triggers for Implementation:**
1. Performance profiling shows formatting as bottleneck
2. REPL implementation shows repeated formatting of same types
3. Users report slow error message generation
4. IDE integration requires sub-millisecond response times

## Alternative Optimizations

Before implementing memoization, consider:
1. **Lazy formatting**: Only format the parts of types that are displayed
2. **Streaming output**: Generate output incrementally
3. **Simplified modes**: Ultra-terse mode that skips formatting entirely
4. **Partial caching**: Cache only expensive operations (e.g., diff finding)

## Implementation Checklist (If Needed)

If memoization becomes necessary:
- [ ] Add ETS table initialization to application startup
- [ ] Implement cache key generation (consider type normalization)
- [ ] Add cache invalidation strategy
- [ ] Add metrics/monitoring for cache hit rates
- [ ] Add configuration option to enable/disable caching
- [ ] Performance test with realistic workloads
- [ ] Document memory usage implications
- [ ] Add cache size limits and eviction policy

## Conclusion

Memoization is a valid optimization but not currently necessary. The existing implementation is performant enough for typical use cases. This decision should be revisited when:
- Real-world usage patterns are established
- Performance metrics are available
- Specific use cases (REPL, IDE) are implemented