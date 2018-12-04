# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: bhamidi <marvin@42.fr>                     +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2018/05/20 19:05:42 by bhamidi           #+#    #+#              #
#    Updated: 2018/12/04 15:47:54 by msrun            ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

NAME1		= ft_turing.native
NAME2		= ft_turing.byte
CAMLOPT		= ocamlopt
CAMLC		= ocamlc
CAMLFLAGS	= -linkpkg -package graphics -package yojson

CMI		= turing.cmi tape.cmi main.cmi
CMX		= turing.cmx tape.cmx main.cmx
CMO		= turing.cmo tape.cmo main.cmo
ML		= turing.mli tape.ml turing.ml main.ml
OBJ		= turing.o tape.o main.o

all: native

native: $(NAME1) Makefile

byte: $(NAME2) Makefile

$(NAME1): $(ML)
	ocamlfind $(CAMLOPT) $(CAMLFLAGS) $(ML) -o $@

$(NAME2): $(ML)
	ocamlfind $(CAMLC) $(CAMLFLAGS) $(ML) -o $@

clean:
	rm -f $(OBJ)
	rm -f $(CMI)
	rm -f $(CMX)
	rm -f $(CMO)

fclean : clean
	rm -f $(NAME1)
	rm -f $(NAME2)

re : fclean all

.PHONY: all fclean clean re
