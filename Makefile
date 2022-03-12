
# Requires docker and circleci installed
# circleci local runner doesn't support built caching so it takes forever
.PHONY: ci
ci:
	circleci config validate
	circleci config process .circleci/config.yml > process.yml
	circleci local execute --job build

# runs unit test making sure warnings don't block test runs
# not suitable for CI which should make warnings block test runs
.PHONY: unit
unit:
	cabal new-test unit --enable-tests --ghc-options=-Wwarn

.PHONY: integration
integration:
	cabal new-test integration --enable-tests --ghc-options=-Wwarn

# runs unit and integration test
.PHONY: test
test: unit integration

