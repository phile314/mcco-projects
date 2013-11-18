CONFIGURE_FLAG=--enable-tests
RUN := runhaskell Setup.hs

all: build

build: dist/setup-config
	$(RUN) build

dist/setup-config:
	$(RUN) configure $(CONFIGURE_FLAG)

configure:
	$(RUN) configure $(CONFIGURE_FLAG)

clean:
	$(RUN) clean

test: build
	$(RUN) test
