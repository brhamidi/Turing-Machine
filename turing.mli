(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   turing.mli                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: bhamidi <marvin@42.fr>                     +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/11/14 16:13:45 by bhamidi           #+#    #+#             *)
(*   Updated: 2018/11/16 18:25:37 by msrun            ###   ########.fr       *)
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

type 'a trying = Some of 'a | Failure of string

val getMachine : string -> string -> t trying
