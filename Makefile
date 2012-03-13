
PREFIX=$(HOME)/devel/install

all: build

setup: Setup.hs
	mkdir -p dist/setup
	ghc --make -o setup -odir dist/setup -hidir dist/setup Setup.hs

.PHONY: build clean

dist/setup-config build install: setup

dist/setup-config: tfp.cabal
	./setup configure --prefix=$(PREFIX) --user -p $(CONFIGURE_FLAGS) -v
build: dist/setup-config
	./setup build -v
install: build
	./setup install
clean:
	rm -vrf setup dist
