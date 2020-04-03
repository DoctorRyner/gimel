default:
	npm i
	spago build
	make dev

dev:
	make -j build build-watch

build:
	npm run build

build-watch:
	npm run build-watch
