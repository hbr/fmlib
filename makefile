.PHONY: all
all: doc test


.PHONY: build
build:
	dune build



.PHONY: doc
doc:
	dune build @doc


.PHONY: sdoc
sdoc:
	make -C src/sdoc/ html



.PHONY: examples

examples:
	make -C src/examples/browser



.PHONY: test
test:
	dune build @runtest



.PHONY: gh-pages-odoc
gh-pages-odoc: doc
	# Generate odoc documentation and push it to gh-pages
	(cd gh-pages; git rm -r -f odoc); \
	mkdir gh-pages/odoc; \
	cp -r _build/default/_doc/_html/* gh-pages/odoc/; \
	(cd gh-pages; git add odoc; git commit --amend --no-edit; git push -f)


.PHONY: gh-pages-webapp
gh-pages-webapp:
	dune build --profile release ./src/examples/browser/single_page_backend.exe; \
	dune build --profile release ./src/examples/browser/circles.js; \
	dune build --profile release ./src/examples/browser/file_select.js; \
	dune build --profile release ./src/examples/browser/flight.js; \
	dune build --profile release ./src/examples/browser/spreadsheet.js; \
	dune build --profile release ./src/examples/browser/temperature.js; \
	dune build --profile release ./src/examples/browser/timer.js; \
	(cd gh-pages; git rm -r -f webapp); \
	mkdir  gh-pages/webapp; \
	cp src/examples/browser/*.js \
	   src/examples/browser/*.exe \
	   src/examples/browser/*.html \
	   src/examples/browser/*.json  \
	   gh-pages/webapp/; \
	(cd gh-pages; git add webapp; git commit --amend --no-edit; git push -f)




#----------------------------------------------------------------------
# opam support
#----------------------------------------------------------------------


# Write 'opam' files to the opam repository
.PHONY: opam opam_rm


version     = 0.6.1
# ---------------------------------------------
opam_repo   = ../opam-repository/packages
dir0        = $(opam_repo)/fmlib/fmlib.$(version)
dir_std     = $(opam_repo)/fmlib_std/fmlib_std.$(version)
dir_pretty  = $(opam_repo)/fmlib_pretty/fmlib_pretty.$(version)
dir_parse   = $(opam_repo)/fmlib_parse/fmlib_parse.$(version)
dir_js      = $(opam_repo)/fmlib_js/fmlib_js.$(version)
dir_browser = $(opam_repo)/fmlib_browser/fmlib_browser.$(version)



# Don't forget to update the version and the 'url' file
#                            ^^^^^^^         ^^^^^
# In the 'url' file the version and the md5sum have to
# be updated!!
# -----------------------------------------------------
opam:
	mkdir $(dir0); \
	cat fmlib.opam url >$(dir0)/opam; \
	mkdir -p $(dir_std); \
	cat fmlib_std.opam url >$(dir_std)/opam; \
	mkdir -p $(dir_pretty); \
	cat fmlib_pretty.opam url >$(dir_pretty)/opam; \
	mkdir -p $(dir_parse); \
	cat fmlib_parse.opam url >$(dir_parse)/opam; \
	mkdir -p $(dir_js); \
	cat fmlib_js.opam url >$(dir_js)/opam
	mkdir -p $(dir_browser); \
	cat fmlib_browser.opam url >$(dir_browser)/opam

opam_rm:
	rm -rf $(dir0); \
	rm -rf $(dir_std); \
	rm -rf $(dir_pretty); \
	rm -rf $(dir_parse); \
	rm -rf $(dir_js);    \
	rm -rf $(dir_browser)




.PHONY: opam_pin_remove opam_remove

opam_pin_remove:
	opam pin remove -n fmlib;         \
	opam pin remove -n fmlib_std;     \
	opam pin remove -n fmlib_pretty;  \
	opam pin remove -n fmlib_parse;   \
	opam pin remove -n fmlib_js;      \
	opam pin remove -n fmlib_browser


opam_remove:
	opam -y remove fmlib;         \
	opam -y remove fmlib_std;     \
	opam -y remove fmlib_pretty;  \
	opam -y remove fmlib_parse;   \
	opam -y remove fmlib_js;      \
	opam -y remove fmlib_browser
