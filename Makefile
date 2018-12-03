# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: bhamidi <marvin@42.fr>                     +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2018/05/20 19:05:42 by bhamidi           #+#    #+#              #
#    Updated: 2018/06/30 13:25:40 by bhamidi          ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

#  ocamlc -o turing  turing.mli tape.ml turing.ml main.ml

NAME		= ft_turing
CAMLOPT		= ocamlopt
CAMLC		= ocamlc
CAMLFLAGS	= -linkpkg -package graphics -package yojson

CMI		= turing.cmi tape.cmi main.cmi
CMX		= turing.cmx tape.cmx main.cmx
CMO		= turing.cmo tape.cmo main.cmo
ML		= turing.mli tape.ml turing.ml main.ml
OBJ		= turing.o tape.o main.o

all: $(NAME)

$(NAME): $(CMI)
	ocamlfind $(CAMLOPT) $(CAMLFLAGS) $(ML) -o $(NAME)

byte: $(CMI)
	ocamlfind $(CAMLC) $(CAMLFLAGS) $(ML) -o $(NAME)

%.cmi: %.ml Makefile
	ocamlfind $(CAMLOPT) $(CAMLFLAGS) $(ML) -c -o $@

clean:
	rm -f $(OBJ)
	rm -f $(CMI)
	rm -f $(CMX)
	rm -f $(CMO)

fclean : clean
	rm -f $(NAME)

re : fclean all

.PHONY: all fclean clean re
