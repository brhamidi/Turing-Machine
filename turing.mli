(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   turing.mli                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: bhamidi <marvin@42.fr>                     +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/11/14 16:13:45 by bhamidi           #+#    #+#             *)
(*   Updated: 2018/11/15 18:51:07 by bhamidi          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type alphabet
type blank
type state
type states
type initial
type finals
type transitions

type t

val getMachine : string -> string -> t Try
