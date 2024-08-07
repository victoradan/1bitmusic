
compile:
	cabal v2-build
	cabal v2-run tepoz-viii
	sox tepoz_viii.wav Tepoz_VIII.flac

profile:
	cabal v2-build
	cabal v2-run tepoz-viii -- +RTS -p -s -hT -l
	sox tepoz_viii.wav Tepoz_VIII.flac
	eventlog2html tepoz-viii.eventlog
	chromium tepoz-viii.eventlog.html
