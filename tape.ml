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

let tape_of_list blank = function
  | hd :: tl -> [], hd, tl
  | [] -> [], blank, []

let current = function
  | _, item, _ -> item

let next blank = function
  | prev, item, next :: next_tl ->
    item :: prev, next, next_tl
  | prev, item, [] -> item :: prev, blank, []

let prev blank = function
  | prev :: prev_tl, item, next ->
    prev_tl, prev, item :: next
  | [], item, next -> [], blank, item :: next

let newCurrent tape newC =
  match tape with
  | prev, _, next -> prev, newC, next

let print tape len blank =
  let rec printerLeft t l =
    if (l > 0) then
      begin
        let newTape = prev blank t in
        printerLeft newTape (l - 1);
        print_char (current newTape);
      end
  in
  let rec printerRight t l =
    if (l > 0) then
      begin
        let newTape = next blank t in
        print_char (current newTape);
        printerRight newTape (l - 1)
      end
  in
  printerLeft tape len;
  print_string "\027[31m";
  print_char (current tape);
  print_string "\027[0m";
  printerRight tape len
