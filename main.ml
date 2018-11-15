(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: bhamidi <marvin@42.fr>                     +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/11/12 15:30:46 by bhamidi           #+#    #+#             *)
(*   Updated: 2018/11/15 18:11:10 by bhamidi          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let usage bin =
  print_string bin ; print_endline " jsonfile input"

let main jsonfile input = 
  match (Turing.fromFile jsonfile) with
  | Some turing ->
    (* if (Turing.input_is_valid ) *) print_endline "wip .."
  | None -> print_endline "json bad formated"

let () =
  match (Array.to_list Sys.argv) with
  | _ :: jsonfile :: input :: [] -> main jsonfile input
  | _ -> usage (Array.get Sys.argv 0)
