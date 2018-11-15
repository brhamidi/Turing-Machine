(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   turing.mli                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: bhamidi <marvin@42.fr>                     +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/11/14 16:13:45 by bhamidi           #+#    #+#             *)
(*   Updated: 2018/11/15 18:11:53 by bhamidi          ###   ########.fr       *)
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

val fromFile : string -> t option

(*
 *
val input_is_valid : alphabet * blank -> string -> bool

val print : t -> unit

val compute : t -> char -> t option
*
*)
