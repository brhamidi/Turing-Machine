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
  Printf.printf "usage:\n\t%s [-h] jsonfile --complexity" bin ;
  Printf.printf "\n\t%s [-h] jsonfile input\n" bin;
  Printf.printf "\npositional arguments:\n";
  Printf.printf "  jsonfile\t\tjson description of the machine\n";
  Printf.printf "  input\t\t\tjson input of the machine\n";
  Printf.printf "  --complexity\t\tdisplay graphic curves of time complexity\n";
  Printf.printf "\noptional arguments:\n";
  Printf.printf "  -h, --help\t\t show this help message and exit\n"

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
  | _ :: "-h" :: _ -> usage (Array.get Sys.argv 0)
  | _ :: "--help" :: _ -> usage (Array.get Sys.argv 0)
  | _ :: jsonfile :: "--complexity" :: [] -> Turing.Complexity.compute jsonfile
  | _ :: jsonfile :: input :: [] -> main jsonfile input
  | _ -> usage (Array.get Sys.argv 0)
