.PHONY: phdlc clean_all clean

phdlc:
	rm -rf _phdlc

	mkdir _phdlc
	mkdir _phdlc/runtime

	dune build

# copy main executable
	cp ../_build/default/hdl-simulator/bin/phdlc.exe _phdlc/phdlc
# copy .cmi files
	cp ../_build/default/hdl-simulator/lib/backend/ocaml/runtime/.runtime.objs/byte/*.cmi _phdlc/runtime/
# copy runtime.cma
	cp ../_build/default/hdl-simulator/lib/backend/ocaml/runtime/runtime.cma _phdlc/runtime/runtime.cma

clean:
	rm -rf _phdlc

clean_all:
	dune clean
	rm -rf _phdlc
