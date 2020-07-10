#   Makefile for Flathub  ><(((º>
#
#
#  Targets:
#
#    all         - (default target) compiles the production build
#    dev         - Compiles in dev mode and doesn't install npm modules
#

# Executables to build using "make all"
EXECUTABLES = install build

all: $(EXECUTABLES)

#
# Individual executables
dev:
	npm run-script build-dev

install:
	npm install

build:
	npm run-script build