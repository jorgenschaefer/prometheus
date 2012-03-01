MAKEINFO=makeinfo
TEXI2DVI=texi2dvi

.PHONY: examples

all: examples prometheus.info prometheus.pdf prometheus.html
	rm examples

# For some reason, TeX can't stand @verbatiminclude ../examples/...
examples:
	test -e examples || ln -s ../examples .

prometheus.info: prometheus.texinfo
	$(MAKEINFO) --no-split $<

prometheus.pdf: prometheus.texinfo
	$(TEXI2DVI) -c -p $<

prometheus.html: prometheus.texinfo
	$(MAKEINFO) --html --no-split $<

clean:
	rm -f prometheus.pdf prometheus.info prometheus.html
