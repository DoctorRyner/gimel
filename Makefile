default:
	npm i
	spago build
	make serve

build:
	make build

serve:
	make -j dev watch

dev:
	npm run dev

watch:
	npm run watch
