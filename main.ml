(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: bhamidi <marvin@42.fr>                     +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/11/12 15:30:46 by bhamidi           #+#    #+#             *)
(*   Updated: 2018/11/21 12:46:42 by msrun            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let usage bin =
  print_string bin ; print_endline " jsonfile --complexity";
  print_string bin ; print_endline " jsonfile input"

let main jsonfile input =
  match (Turing.getMachine jsonfile input) with
  | Failure error -> print_endline error
  | Some m ->
    begin
      print_endline "***** machine description *****";
      print_endline "                               ";
      print_endline "*******************************";
      Turing.printDescription m;
      print_endline "\n***** machine computing *****";
      print_endline "                               ";
      print_endline "*******************************";
      Turing.compute m;
      print_char '\n'
    end

let () =
  match (Array.to_list Sys.argv) with
  | _ :: jsonfile :: "--complexity" :: [] -> Turing.print_time_complexity jsonfile
  | _ :: jsonfile :: input :: [] -> main jsonfile input
  | _ -> usage (Array.get Sys.argv 0)
