IN_DOCKER := docker run -u root --rm -ti -v "$(shell pwd)/bundle:/src/_build" 2bash

all: build
	$(IN_DOCKER) jbuilder build
shell: build
	$(IN_DOCKER) bash
build:
	docker build -t "2bash" .
mac:
	jbuild build
test:
	jbuild runtest
