# Program main file

# Ocamlbuild
OCBOPTS=-use-ocamlfind -pkg unix,menhirLib,why3,core,oUnit,sedlex -tag debug,thread  -cflags -g
OCAMLBUILD=ocamlbuild $(OCBOPTS)

VERSION=byte


NAME=interpreter
# Program main file
MAIN=src/$(NAME)


.PHONY: $(NAME) clean clean-py $(NAME).byte $(NAME).native $(NAME).d.byte $(NAME).d.native 

all: $(NAME)

$(NAME): $(NAME).$(VERSION)
	cp $(NAME).$(VERSION) $(NAME)


runtime:
	$(MAKE) -C runtime


$(NAME).byte:
	$(OCAMLBUILD) $(MAIN).byte

$(NAME).native:
	$(OCAMLBUILD) $(MAIN).native

$(NAME).p.byte:
	$(OCAMLBUILD) $(MAIN).p.byte

$(NAME).d.byte:
	$(OCAMLBUILD) $(MAIN).d.byte

$(NAME).p.native:
	$(OCAMLBUILD) $(MAIN).p.native

clean::
	$(OCAMLBUILD) -clean
	rm -f parser.conflicts
	rm -rf $(NAME) $(NAME).byte $(NAME).exe TAGS



