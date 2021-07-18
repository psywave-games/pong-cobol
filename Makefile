.PHONY: run

run:
	cobc -xjd game.cbl -lraylib

game:
	cobc -xd game.cbl -lraylib