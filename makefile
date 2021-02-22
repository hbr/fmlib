.PHONY: all
all: doc test



.PHONY: doc
doc:
	dune build @doc



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
