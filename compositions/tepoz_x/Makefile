
NUM=x

video:
	convert -size 1200x400 xc:black -gravity center -font 'Utopia' -pointsize 150 -fill white -annotate +0+0 "Tepoz $(shell echo ${NUM} | tr '[:lower:]' '[:upper:]')" title_image.png
	ffmpeg -loop 1 -i title_image.png -i tepoz_${NUM}.flac -c:v libx264 -pix_fmt yuv420p -tune stillimage -c:a copy -strict -2  -shortest tepoz_${NUM}.mp4 

compile:
	cabal v2-build
	cabal v2-run tepoz-${NUM}
	sox tepoz_${NUM}.wav tepoz_${NUM}.flac

profile:
	cabal v2-build
	cabal v2-run tepoz-${NUM} -- +RTS -p -s -hc -l
	eventlog2html tepoz-${NUM}.eventlog
	chromium tepoz-${NUM}.eventlog.html

play:
	play tepoz_${NUM}.flac