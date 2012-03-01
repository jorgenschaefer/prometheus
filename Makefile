.PHONY: all doc

all: doc

doc:
	$(MAKE) -C doc

clean:
	$(MAKE) -C doc clean
