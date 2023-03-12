# Makefile
# Commands in this makefile are meant for local development, not for CI.
# The cabal file errors on warnings, but these commands do not elevate warnings to errors for ease of development.

# Requires docker and circleci installed
# circleci local runner doesn't support built caching so it takes forever
.PHONY: ci
ci:
	circleci config validate
	circleci config process .circleci/config.yml > process.yml
	circleci local execute --job build

.PHONY: build
build:
	cabal build --ghc-options=-Wwarn

.PHONY: unit
unit:
	cabal test unit --ghc-options=-Wwarn

.PHONY: integration
integration:
	cabal test integration --ghc-options=-Wwarn

# runs all test suites
.PHONY: test
test:
	cabal test --ghc-options=-Wwarn
