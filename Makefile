VERSION = 1.3.1
export VERSION

ifndef PREFIX
  PREFIX = $(shell dirname $(shell dirname `which ocamlc`))
endif
ifndef BINDIR
  BINDIR = $(PREFIX)/bin
endif
ifndef OCAMLC
  OCAMLC = ocamlc -w A
endif
ifndef OCAMLOPT
  OCAMLOPT = ocamlopt
endif
ifndef OCAMLLEX
  OCAMLLEX = ocamllex
endif
ifndef OCAMLDEP
  OCAMLDEP = ocamldep
endif

MODULES = hashtbl2 version annot tag plugin input output main

OBJS = $(patsubst %, %.cmo, $(MODULES))
OBJS-NAT = $(patsubst %, %.cmx, $(MODULES))

.PHONY: default
default: caml2html test

### GODI targets ###
.PHONY: all opt install
all: byte bytelib
opt: caml2html optlib
install:
	install -m 0755 caml2html $(BINDIR) || \
		install -m 0755 caml2html.byte $(BINDIR)/caml2html
	test -f caml2html.cma -o -f caml2html.cmxa && $(MAKE) libinstall
uninstall:
	rm -f $(BINDIR)/caml2html
	$(MAKE) libuninstall || true
### end of GODI targets ###

.PHONY: pre byte test lib libinstall libuninstall \
        bytelib optlib tidy clean dep archive

pre: version.ml caml2html.mli caml2html.ml
caml2html.mli: annot.mli plugin.mli input.mli output.mli version.ml \
		caml2html.mli.mlx
	camlmix -clean caml2html.mli.mlx -o caml2html.mli
caml2html.ml: hashtbl2.mli hashtbl2.ml tag.ml annot.ml \
              plugin.ml input.ml output.ml caml2html.ml.mlx
	camlmix -clean caml2html.ml.mlx -o caml2html.ml
version.ml: version.ml.mlx Makefile
	camlmix -clean version.ml.mlx -o version.ml

byte: caml2html.byte

test:
	ocamlc -i -dtypes caml2html_test.ml > caml2html_test.mli
#	ocamlc -i -dtypes -pp 'camlp5o pa_extend.cmo q_MLast.cmo -loc _loc' \
#		-I +camlp5 caml2html_test2.ml > caml2html_test2.mli
	./caml2html -o caml2html_test.html \
		caml2html_test.mli caml2html_test.ml caml2html_test2.ml \
		-ln -ie7 \
		-ext date:date \
		-ext cat:cat \
		-ext "rot13:tr '[a-z]' '[n-za-m]'"
	./caml2html -o caml2html_self_test.html \
		tag.ml annot.mli annot.ml plugin.mli plugin.ml \
		input.mli input.mll output.mli output.ml \
		main.ml \
		-ln

caml2html: $(OBJS-NAT)
	$(OCAMLOPT) -o caml2html str.cmxa unix.cmxa $(OBJS-NAT)

caml2html.byte: $(OBJS)
	$(OCAMLC) -custom -o caml2html.byte str.cma unix.cma $(OBJS)

lib: all bytelib optlib

libinstall:
	ocamlfind install caml2html META caml2html.mli caml2html.cmi \
		caml2html.*a

libuninstall:
	ocamlfind remove caml2html

bytelib: $(OBJS) caml2html.cmi caml2html.cmo
	$(OCAMLC) -a -o caml2html.cma caml2html.cmo

optlib: $(OBJS-NAT) caml2html.cmi caml2html.cmx
	$(OCAMLOPT) -a -o caml2html.cmxa caml2html.cmx

tidy:
	rm -f caml2html caml2html.byte
	rm -f *.cm[ixoa] *.cmxa *.a *.obj *.o *~ *.annot
	rm -f *.ml.html caml2html_test.html caml2html_self_test.html

clean: tidy
	rm -f input.ml *.mlx.ml
	rm -f caml2html.ml caml2html.mli version.ml
	rm -f caml2html.html caml2html-help

dep: input.ml
	$(OCAMLDEP) hashtbl2.mli hashtbl2.ml version.ml annot.mli annot.ml \
		tag.ml plugin.mli plugin.ml input.mli input.ml \
		output.mli output.ml main.ml > depend

.SUFFIXES:
.SUFFIXES: .mll .mly .ml .mli .cmi .cmo .cmx

.mll.ml:
	$(OCAMLLEX) $<
.mly.ml:
	$(OCAMLYACC) $<
.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmo:
	$(OCAMLC) -dtypes -c $<
.ml.cmx:
	$(OCAMLOPT) -dtypes -c $<

-include depend

input.ml: input.mll


################ Only for developers
P = caml2html-$(VERSION)

caml2html.html: caml2html caml2html.html.mlx
	./caml2html -help > caml2html-help
	camlmix -o caml2html.html caml2html.html.mlx

archive: pre opt test caml2html.html
	@echo "Making archive for version $(VERSION)"
	rm -rf /tmp/$(P) && \
	 	cp -rp . /tmp/$(P) && \
		cd /tmp/$(P) && $(MAKE) tidy && \
			rm -f *~ caml2html*.tar* && \
		cd .. && tar czf $(P).tar.gz $(P) && \
		tar cjf $(P).tar.bz2 $(P)
	mv /tmp/$(P).tar.gz /tmp/$(P).tar.bz2 .
	cp $(P).tar.gz $(P).tar.bz2 $$WWW/
	cp $(P).tar.gz $(P).tar.bz2 ../releases/
	cd $$WWW/ && ln -sf $(P).tar.gz caml2html.tar.gz && \
		ln -sf $(P).tar.bz2 caml2html.tar.bz2
	cp caml2html.html $$WWW/caml2html-help.html
	cp README $$WWW/caml2html-readme.txt
	cp history.txt $$WWW/caml2html-history.txt
	cp version.ml $$WWW/caml2html-version.ml
	cp caml2html_test.ml $$WWW/
	cp caml2html_test.html $$WWW/
	touch -c $$WWW/caml2html.html.mlx
