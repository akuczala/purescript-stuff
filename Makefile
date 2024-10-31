check:
	spago build
	purs-tidy format-in-place src/*.purs

build:
	spago bundle --bundle-type app --outfile dist/Main.js