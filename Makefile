.PHONY:	all clean byte native profile debug test

OCB_FLAGS = -pkg yojson -tag bin_annot
OCB = 		ocamlbuild $(OCB_FLAGS)

all: native byte # profile debug

clean:
		$(OCB) -clean

native:
		$(OCB) main.native

byte:
		$(OCB) main.byte

profile:
		$(OCB) -tag profile main.native

debug:
		$(OCB) -tag debug main.byte

run: native
		./main.native

native-test:
		$(OCB) test.native

test: native-test
		./test.native
