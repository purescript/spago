all: build

fetch-templates:
	@./scripts/fetch-templates

dev: fetch-templates
	@stack build --fast --file-watch

build: fetch-templates
	@stack build --fast

test: fetch-templates
	@stack test --fast --pedantic

install: build
	@stack install --fast