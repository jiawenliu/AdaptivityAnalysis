#!/bin/sh
ocamllex Lexer.mll && \
ocamlyacc Parser.mly && \
ocamlc -c Ast.ml && \
ocamlc -c Parser.mli && \
ocamlc -c Parser.ml && \
ocamlc -c Lexer.ml && \
ocamlc -c Main.ml && \
ocamlmktop -o be.top Ast.cmo Parser.cmo Lexer.cmo Main.cmo