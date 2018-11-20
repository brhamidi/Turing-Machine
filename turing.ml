(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   turing.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: bhamidi <marvin@42.fr>                     +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/11/14 16:37:19 by bhamidi           #+#    #+#             *)
(*   Updated: 2018/11/20 18:52:55 by msrun            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type alphabet = char list
type blank = char
type state = string
type states = state list
type initial = state
type finals = state list

module StateMap = Map.Make(String)
module CharMap = Map.Make(Char)

type transitions = (string * char * string) CharMap.t StateMap.t

type descriptions =
  { name : string;
    alphabet : alphabet;
    blank : blank;
    states : states;
    initial : initial;
    finals : finals;
    transitions: transitions;
  }

type t = (Tape.t * descriptions)

type 'a trying = Some of 'a | Failure of string

exception Parsing_error of string

let getTransitions l =
  let f x y = match y with
    | `Assoc [("read", `String a); ("to_state", `String b); ("write", `String c); ("action", `String d)] ->
      begin
        match d with
        | "LEFT" | "RIGHT" -> CharMap.add (String.get a 0) ( b, String.get c 0, d) x
        | _ -> raise (Parsing_error ("Error while parsing transitions field, wrong action: " ^ d))
end
    | _ -> raise (Parsing_error "Error while parsing transitions field.")
  in
  let fillCmap l = List.fold_left (fun x y -> f x y) CharMap.empty l
  in
  let fillSmap l =
    List.fold_left (fun x y -> match y with | (transi, lt) ->
    StateMap.add transi (fillCmap ( match lt with | `List x -> x | _ -> raise (Parsing_error "Error while parsing transitions field."))) x) StateMap.empty l
  in
  fillSmap l

let getStr str =
  match str with
  | `String s -> s
  | _ -> raise (Parsing_error "Error while parsing string field.")

let getAlphabet l =
  let lA = match l with
    | `List l -> l
    | _ -> raise (Parsing_error "Error while parsing alphabet field.") in
  List.fold_left (fun x y -> (match y with | `String x -> String.get x 0 | _ -> raise (Parsing_error "Error while parsing alphabet field.")) :: x) [] lA

let getBlank str =
  match str with
  | `String s -> String.get s 0
  | _ -> raise (Parsing_error "Error while parsing blank field.")

let getListStr lstr =
  let lS =
    match lstr with
    | `List l -> l
    | _ -> raise (Parsing_error "Error while parsing list of string.") in
  List.fold_left (fun x y -> (match y with | `String x -> x | _ -> raise (Parsing_error "Error while parsing list of string.")) :: x) [] lS

let callFunction f field json =
  f (Yojson.Basic.Util.member field json)

let getDescrition name : descriptions trying =
  let t = Yojson.Basic.from_file name in
  let getTran (x : Yojson.Basic.json) =
    match x with
    | (`Assoc y ) -> getTransitions y
    | _ -> raise (Parsing_error "Error while parsing, no transitions.")
  in
  try Some {
    name = callFunction getStr "name" t;
    alphabet = callFunction getAlphabet "alphabet" t;
    blank = callFunction getBlank "blank" t;
    states = callFunction getListStr "states" t;
    initial = callFunction getStr "initial" t;
    finals = callFunction getListStr "finals" t;
    transitions = callFunction getTran "transitions" t
  } with
  | Parsing_error err -> Failure err
  | _ -> Failure "Error while parsing input"

let list_from_string str =
  let rec get_list s i l =
    if (i < 0)
    then l
    else get_list s (i - 1) ((String.get s i) :: l)
  in
  get_list str ((String.length str) - 1) []

let search_alphabet_opt alphabet letter =
  List.find_opt (fun x -> (x = letter)) alphabet

let search_alphabet alphabet letter =
  match List.find (fun x -> (x = letter)) alphabet with
  | _ -> ()

let search_state x states =
  match List.find (fun y -> y = x) states with
  | _ -> ()

let check_description d =
  try match search_alphabet_opt d.alphabet d.blank with
    | Some _ ->
      begin
        try List.iter (fun x ->
            match search_state x d.states with
            | _ -> ()) d.finals
        with
        | _ -> raise (Parsing_error "Error finals state not valid")
      end;
      begin
        try (StateMap.iter (fun x y ->
            begin
              match List.find (fun z -> z = x) d.states with
              | _ -> ()
            end;
            begin
              CharMap.iter (fun a b -> search_alphabet d.alphabet a;
                             match b with
                             | (f, g, h) ->
                               begin
                                 search_state f d.states;
                                 search_alphabet d.alphabet g;
                                 match h with
                                 | "LEFT" | "RIGHT" -> ()
                                 | _ -> raise (Parsing_error "Error action")
                               end
                           ) y
            end
          ) d.transitions); Some "ok" with
        | Parsing_error e -> Failure e
        | _ -> Failure "Error in transitions"
      end
    | None -> Failure "Error no blank in alphabet"
  with
  | Parsing_error e -> Failure e
  | _ -> Failure "Error description not valid"

let check_input str blank =
  let rec is_there_blank s i =
    if (i >= 0)
    then
      begin
        if (String.get s i = blank)
        then false
        else is_there_blank s (i - 1)
      end
    else
      true
  in
  is_there_blank str ((String.length str) - 1)

let getMachine jsonfile input : t trying =
  let desc = getDescrition jsonfile in
  match desc with
  | Failure e -> Failure e
  | Some description ->
    begin
      match (check_description description) with
      | Failure x -> print_string x
      | _ -> print_string "ok"
    end;
    if (check_input input description.blank)
    then Some (Tape.tape_of_list (list_from_string input), description)
    else Failure "Error there is blank in input."


