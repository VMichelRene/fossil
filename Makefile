OCB_FLAGS = -pkgs unix \
-I file_bit \
-I fossil \
-I outils \
-I tree 


OCB       = ocamlbuild $(OCB_FLAGS)

test:\
test_bit.byte \
test_file_bit.byte \
test_fossil.byte \
test_outils.byte

clean:
	ocamlbuild -clean 

.SUFFIXES:
.SUFFIXES: .ml .byte 

.ml.byte:
	$(OCB)  $*.byte

