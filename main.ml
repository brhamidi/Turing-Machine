(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: bhamidi <marvin@42.fr>                     +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/11/12 15:30:46 by bhamidi           #+#    #+#             *)
(*   Updated: 2018/11/15 19:01:27 by bhamidi          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let usage bin =
  print_string bin ; print_endline " jsonfile input"

let main jsonfile input = 
  match (Turing.getMachine jsonfile input) with
  | Failure error -> print_endline error
  | Some machine -> print_endline "Running Machine .."

let () =
  match (Array.to_list Sys.argv) with
  | _ :: jsonfile :: input :: [] -> main jsonfile input
  | _ -> usage (Array.get Sys.argv 0)
