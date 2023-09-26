
run:
	stack run 1bitmusic-exe

stats:
	stack run -- 1bitmusic-exe +RTS -s

profile:
	stack build --profile
	stack exec --profile -- 1bitmusic-exe +RTS -p