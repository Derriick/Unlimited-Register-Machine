APP1 = ex1
APP2 = ex2

CAMLC = ocamlc
.PRECIOUS: %.cmi %.cmo

SRC = $(wildcard *.mli)
INT = $(SRC:.mli=.cmi)
OBJ = $(SRC:.mli=.cmo)

all: $(APP1) $(APP2)

%.cmi: %.mli
	$(CAMLC) -c $<

%.cmo: %.ml $(INT)
	$(CAMLC) -c $<

%: $(OBJ) %.cmo
	$(CAMLC) -o $@ $^

clean:
	rm -rf $(APP1) $(APP2) *.cm[io] 2>/dev/null
