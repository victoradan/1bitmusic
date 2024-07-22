
NUM=ix

compile:
	cabal v2-build
	cabal v2-run tepoz-${NUM}
	sox tepoz_${NUM}.wav tepoz_${NUM}.flac

profile:
	cabal v2-build
	cabal v2-run tepoz-${NUM} -- +RTS -p -s -hc -l
	sox tepoz_${NUM}.wav tepoz_${NUM}.flac
	eventlog2html tepoz-${NUM}.eventlog
	chromium tepoz-${NUM}.eventlog.html

play:
	play tepoz_${NUM}.flac