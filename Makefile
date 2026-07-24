# Catena Compiler Makefile
# Thin convenience wrappers around the canonical rebar3 workflow

REBAR3 ?= rebar3

.PHONY: help check-modules check-specs conformance verify compile test coverage coverage-report clean

help:
	@echo "Catena Compiler - Available targets:"
	@echo ""
	@echo "  make check-modules   - Verify Erlang module names are unique"
	@echo "  make check-specs     - Validate specs governance and evidence mappings"
	@echo "  make conformance     - Validate specs and run scenario evidence"
	@echo "  make verify          - Validate specs and run the complete test suite"
	@echo "  make compile         - Compile all source modules"
	@echo "  make test            - Run all tests"
	@echo "  make coverage        - Run tests with coverage reporting"
	@echo "  make coverage-report - Show coverage summary (after running coverage)"
	@echo "  make clean           - Remove build artifacts"
	@echo ""

check-modules:
	@./scripts/check_module_names.sh

check-specs: compile
	@escript scripts/check_specs.escript

conformance: check-specs
	@modules=$$(awk -F '\t' 'NR > 1 { print $$2 }' specs/conformance/executable_scenarios.tsv | sort -u | paste -sd, -); \
	test -n "$$modules"; \
	$(REBAR3) eunit --module="$$modules"

verify: check-specs
	@$(MAKE) --no-print-directory test

compile: check-modules
	@$(REBAR3) compile

test: check-modules
	@$(REBAR3) eunit

coverage: check-modules
	@$(REBAR3) eunit --cover
	@$(REBAR3) cover --verbose

coverage-report:
	@$(REBAR3) cover --verbose

clean:
	@$(REBAR3) clean
