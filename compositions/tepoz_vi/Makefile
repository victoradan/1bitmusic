
compile:
	cabal v2-build
	cabal v2-run tepoz-vi
	sox tepoz_vi.wav Tepoz_VI.flac

profile:
	cabal v2-build
	cabal v2-run tepoz-vi -- +RTS -p -s -hT -l
	sox tepoz_vi.wav Tepoz_VI.flac
	eventlog2html tepoz-vi.eventlog
	chromium tepoz-vi.eventlog.html
