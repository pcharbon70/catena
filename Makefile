# Catena Compiler Makefile
# Provides convenient targets for building, testing, and coverage

.PHONY: help compile test coverage coverage-report clean

help:
	@echo "Catena Compiler - Available targets:"
	@echo ""
	@echo "  make compile         - Compile all source modules"
	@echo "  make test            - Run all tests"
	@echo "  make coverage        - Run tests with coverage reporting"
	@echo "  make coverage-report - Show coverage summary (after running coverage)"
	@echo "  make clean           - Remove build artifacts"
	@echo ""

# Compile all source modules
compile:
	@echo "Compiling source modules..."
	@mkdir -p _build/test
	@erlc -o _build/test -pa _build/test -I src \
		src/compiler/types/catena_types.erl \
		src/compiler/types/catena_type_subst.erl \
		src/compiler/types/catena_type_scheme.erl \
		src/compiler/types/catena_type_env.erl \
		src/compiler/types/catena_type_pp.erl \
		src/compiler/types/catena_type_error.erl \
		src/compiler/types/catena_ast.erl \
		src/compiler/types/catena_config.erl \
		src/compiler/types/catena_constraint.erl \
		src/compiler/types/catena_instance.erl \
		src/compiler/types/catena_coherence.erl \
		src/compiler/types/catena_handler_verify.erl \
		src/compiler/types/catena_infer_state.erl \
		src/compiler/types/catena_infer_unify.erl \
		src/compiler/types/catena_infer_pattern.erl \
		src/compiler/types/catena_infer_expr.erl \
		src/compiler/types/catena_infer_effect.erl \
		src/compiler/types/catena_infer.erl \
		src/compiler/catena_compiler_utils.erl
	@echo "✓ Compilation complete"

# Run all tests
test: compile
	@echo "Compiling test modules..."
	@erlc -o _build/test -pa _build/test -I src \
		test/compiler/types/catena_types_tests.erl \
		test/compiler/types/catena_type_subst_tests.erl \
		test/compiler/types/catena_type_scheme_tests.erl \
		test/compiler/types/catena_type_env_tests.erl \
		test/compiler/types/catena_type_pp_tests.erl \
		test/compiler/types/catena_type_integration_tests.erl \
		test/compiler/types/catena_type_error_tests.erl \
		test/compiler/types/catena_constraint_tests.erl \
		test/compiler/types/catena_instance_tests.erl \
		test/compiler/types/catena_coherence_tests.erl \
		test/compiler/types/catena_handler_verify_tests.erl \
		test/compiler/types/catena_infer_state_tests.erl \
		test/compiler/types/catena_infer_unify_tests.erl \
		test/compiler/types/catena_infer_pattern_tests.erl \
		test/compiler/types/catena_infer_expr_tests.erl \
		test/compiler/types/catena_infer_effect_tests.erl \
		test/compiler/types/catena_infer_tests.erl \
		test/compiler/types/catena_infer_row_unify_tests.erl \
		test/compiler/types/catena_type_subst_occurs_tests.erl
	@echo ""
	@echo "Running tests..."
	@erl -noshell -pa _build/test -eval " \
		eunit:test([ \
			catena_types_tests, \
			catena_type_subst_tests, \
			catena_type_scheme_tests, \
			catena_type_env_tests, \
			catena_type_pp_tests, \
			catena_type_integration_tests, \
			catena_type_error_tests, \
			catena_infer_state_tests, \
			catena_infer_unify_tests, \
			catena_infer_pattern_tests, \
			catena_infer_expr_tests, \
			catena_infer_effect_tests, \
			catena_infer_tests, \
			catena_infer_row_unify_tests, \
			catena_type_subst_occurs_tests \
		], [verbose]) \
	" -s init stop

# Run tests with coverage
coverage:
	@chmod +x scripts/run_coverage.sh
	@./scripts/run_coverage.sh

# Show coverage summary
coverage-report:
	@chmod +x scripts/coverage_summary.sh
	@./scripts/coverage_summary.sh

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf _build
	@echo "✓ Clean complete"
