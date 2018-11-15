(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   tape.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: bhamidi <marvin@42.fr>                     +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/11/14 17:53:30 by bhamidi           #+#    #+#             *)
(*   Updated: 2018/11/15 19:01:26 by bhamidi          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


type t = char list * char * char list

let tape_of_list = function
  | hd::tl -> [], hd, tl
  | [] -> invalid_arg "empty list"

let current = function
  | _, item, _ -> item

let next = function
  | prev, item, next::next_tl ->
    item::prev, next, next_tl
  | _ ->
    failwith "end of tape reached"

let prev = function
  | prev::prev_tl, item, next ->
    prev_tl, prev, item::next
  | _ ->
    failwith "begin of tape reached"
