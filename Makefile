.PHONY: test

test:
	mix test

test-ssl:
	mix test --only ssl

test-build:
	docker build . -f Dockerfile.test -t smppex-test

test-docker: test-build
	docker run smppex-test

test-docker-ssl: test-build
	docker run smppex-test mix test --only ssl
