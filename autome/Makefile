MAKEFILE_PATH := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
OUTDIR ?= .

.PHONY: all
all: autome

autome:
	cl-launch --output $(OUTDIR)/autome --dump !    \
	--quicklisp                                     \
	--source-registry $(MAKEFILE_PATH)              \
	--system autome                                 \
	--dispatch-entry vol/autome/volume:cli          \
	--dispatch-entry backlight/autome/backlight:cli \
	--dispatch-entry puburl/autome/puburl:cli       \
	--dispatch-entry defaults/autome/defaults:cli   \
