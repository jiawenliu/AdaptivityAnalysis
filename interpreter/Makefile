# Program main file

# Ocamlbuild
OCBOPTS=-use-menhir
OCAMLBUILD=ocamlbuild 

VERSION=byte


NAME=main
# Program main file
MAIN=$(NAME)


.PHONY: $(NAME) clean clean-py $(NAME).byte $(NAME).native 

all: $(NAME)

$(NAME): $(NAME).$(VERSION)
	cp $(NAME).$(VERSION) $(NAME)


runtime:
	$(MAKE) -C runtime


$(NAME).byte:
	$(OCAMLBUILD) $(OCBOPTS) $(MAIN).byte

$(NAME).native:
	$(OCAMLBUILD) $(MAIN).native


clean::
	$(OCAMLBUILD) -clean