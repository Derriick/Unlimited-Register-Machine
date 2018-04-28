APP = app

CAMLC = ocamlc
.PRECIOUS: %.cmi %.cmo

SRC = $(wildcard *.mli)
INT = $(SRC:.mli=.cmi)
OBJ = $(SRC:.mli=.cmo)

all: $(APP)

%.cmi: %.mli
	$(CAMLC) -c $<

%.cmo: %.ml $(INT)
	$(CAMLC) -c $<

%: $(OBJ) %.cmo
	$(CAMLC) -o $@ $^

clean:
	rm -rf $(APP) *.cm[io] 2>/dev/null
