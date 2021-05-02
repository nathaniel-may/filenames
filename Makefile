
# Requires docker and circleci installed
# circleci local runner doesn't support built caching so it takes forever
.PHONY: ci
ci:
	circleci config validate
	circleci config process .circleci/config.yml > process.yml
	circleci local execute --job build

.PHONY: unit
unit:
	cabal new-test unit --enable-tests