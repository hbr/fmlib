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
	dune build --profile release ./src/examples/browser/webapp.js; \
	dune build --profile release ./src/examples/browser/timer.js; \
	(cd gh-pages; git rm -r -f webapp); \
	mkdir  gh-pages/webapp; \
	cp src/examples/browser/*.js \
	   src/examples/browser/*.html \
	   src/examples/browser/*.json  \
	   gh-pages/webapp/; \
	(cd gh-pages; git add webapp; git commit --amend --no-edit; git push -f)






# Write 'opam' files to the opam repository
.PHONY: opam opam_rm

version     = 0.5.4
opam_repo   = ../opam-repository/packages
dir0        = $(opam_repo)/fmlib/fmlib.$(version)
dir_std     = $(opam_repo)/fmlib_std/fmlib_std.$(version)
dir_pretty  = $(opam_repo)/fmlib_pretty/fmlib_pretty.$(version)
dir_parse   = $(opam_repo)/fmlib_parse/fmlib_parse.$(version)
dir_js      = $(opam_repo)/fmlib_js/fmlib_js.$(version)
dir_browser = $(opam_repo)/fmlib_browser/fmlib_browser.$(version)

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
