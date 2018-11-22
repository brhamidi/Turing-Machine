(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   tape.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: bhamidi <marvin@42.fr>                     +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/11/14 17:53:30 by bhamidi           #+#    #+#             *)
(*   Updated: 2018/11/22 17:27:02 by msrun            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


type t = char list * char * char list

let tape_of_list = function
  | hd :: tl -> [], hd, tl
  | [] -> [], '.', []

let current = function
  | _, item, _ -> item

let next = function
  | prev, item, next :: next_tl ->
    item :: prev, next, next_tl
  | prev, item, [] -> item :: prev, '.', []

let prev = function
  | prev :: prev_tl, item, next ->
    prev_tl, prev, item :: next
  | [], item, next -> [], '.', item :: next

let newCurrent tape newC =
  match tape with
  | prev, _, next -> prev, newC, next

let print tape len =
  let rec printerLeft t l =
    if (l > 0)
    then
      (
        let newTape = prev t in
        printerLeft newTape (l - 1);
        print_char (current newTape);
      )
  in
  let rec printerRight t l =
    if (l > 0)
    then
      (
        let newTape = next t in
        print_char (current newTape);
        printerRight newTape (l - 1)
      )
  in
  printerLeft tape len;
  print_string "\027[31m";
  print_char (current tape);
  print_string "\027[0m";
  printerRight tape len;
