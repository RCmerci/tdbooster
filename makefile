.PHONY: web
web:
	dune build @web/default
	cat _build/default/web/index.html > index.html
