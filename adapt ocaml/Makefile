# Program main file

# Ocamlbuild
OCBOPTS=-use-ocamlfind -pkg unix,menhirLib,core,oUnit,sedlex -tag debug,thread  -cflags -g
OCAMLBUILD=ocamlbuild 

VERSION=byte


NAME=tworound
# Program main file
MAIN=$(NAME)


.PHONY: $(NAME) clean clean-py $(NAME).byte $(NAME).native 

all: $(NAME)

$(NAME): $(NAME).$(VERSION)
	cp $(NAME).$(VERSION) $(NAME)


runtime:
	$(MAKE) -C runtime


$(NAME).byte:
	$(OCAMLBUILD) $(MAIN).byte

$(NAME).native:
	$(OCAMLBUILD) $(MAIN).native


clean::
	$(OCAMLBUILD) -clean



