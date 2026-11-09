.PHONY: ghdls clean_all clean

ghdls:
	rm -rf _ghdls

	mkdir _ghdls
	mkdir _ghdls/runtime

	dune build

# copy main executable
	cp ../_build/default/hdl-simulator/bin/ghdls.exe _ghdls/ghdls
# copy .cmi files
	cp ../_build/default/hdl-simulator/lib/backend/ocaml/runtime/.runtime.objs/byte/*.cmi _ghdls/runtime/
# copy runtime.cma
	cp ../_build/default/hdl-simulator/lib/backend/ocaml/runtime/runtime.cma _ghdls/runtime/runtime.cma

clean:
	rm -rf _ghdls

clean_all:
	dune clean
	rm -rf _ghdls
