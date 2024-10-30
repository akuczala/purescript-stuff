check:
	spago build
	purs-tidy format-in-place src/*.purs