MAKEFILE_PATH := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

.PHONY: autome

autome:
	OUTDIR=$(MAKEFILE_PATH)bin/blob make -C $(MAKEFILE_PATH)autome
