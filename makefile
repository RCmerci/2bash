IN_DOCKER := docker run -u root --rm -ti -v "$(shell pwd)/bundle:/src/_build" 2bash

all: build
	$(IN_DOCKER) jbuilder build
shell: build
	$(IN_DOCKER) bash
build:
	docker build -t "2bash" .
mac:
	jbuilder build
.PHONY: test
test:
	jbuilder runtest

mac_install: mac
	cp _build/default/bin/sbash.exe /usr/local/bin/sbash.exe
