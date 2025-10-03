
image:
	# ffmpeg -loop 1 -i Tepoz.png -i AUDIO -c:v libx264 -tune stillimage -c:a flac -b:a 192k -pix_fmt yuv420p -shortest output_video.mp4
	# ffmpeg -loop 1 -i Tepoz.png -i ${AUDIO} -c:v libx264 -tune stillimage -vf "scale=1920:1080,format=yuv420p" -c:a aac -b:a 192k -shortest output_video.mp4


video:
	magick convert Tepoz.png -font 'Palatino' -pointsize 150 -gravity west \
		-fill "rgba(100,100,100,0.75)" -annotate +550+0 Tepoz \
		-fill "rgba(155,100,0,0.75)" -annotate +980+0 "${TEXT}" \
		Tepoz_f.png
	ffmpeg -loop 1 -i Tepoz_f.png -i ${AUDIO} -c:v libx264 -tune stillimage -crf 18 -preset medium -vf "scale=1920:1080,format=yuv420p" -c:a copy -shortest -movflags +faststart Tepoz_${TEXT}.mp4

	# convert input.jpg -gravity center -fill orange -pointsize 72 -annotate 0 "Your Text Here" output.jpg
	# convert -size 1920x1080 xc:black -gravity center -font 'Palatino' -pointsize 150 -fill white -annotate +0+0 "Tepoz $(shell echo ${NUM} | tr '[:lower:]' '[:upper:]')" title_image.png
	# ffmpeg -loop 1 -i Tepoz.png -i ${AUDIO} -c:v libx264 -tune stillimage -crf 18 -preset medium -vf "scale=1920:1080,format=yuv420p" -c:a copy -shortest -movflags +faststart output_video.mp4
