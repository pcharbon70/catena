# Catena Compiler Makefile
# Thin convenience wrappers around the canonical rebar3 workflow

REBAR3 ?= rebar3

.PHONY: help check-modules compile test coverage coverage-report clean

help:
	@echo "Catena Compiler - Available targets:"
	@echo ""
	@echo "  make check-modules   - Verify Erlang module names are unique"
	@echo "  make compile         - Compile all source modules"
	@echo "  make test            - Run all tests"
	@echo "  make coverage        - Run tests with coverage reporting"
	@echo "  make coverage-report - Show coverage summary (after running coverage)"
	@echo "  make clean           - Remove build artifacts"
	@echo ""

check-modules:
	@./scripts/check_module_names.sh

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
