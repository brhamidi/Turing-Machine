(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   turing.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: bhamidi <marvin@42.fr>                     +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/11/14 16:37:19 by bhamidi           #+#    #+#             *)
(*   Updated: 2018/11/22 17:41:19 by msrun            ###   ########.fr       *)
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

type move = Right | Left

exception Parsing_error of string

exception Error

let list_from_string str =
  let rec get_list s i l =
    if (i < 0) then l
    else get_list s (i - 1) ((String.get s i) :: l) in
  get_list str ((String.length str) - 1) []

let changeTape (_, desc) input : t =
  let newTape = Tape.tape_of_list desc.blank (list_from_string input) in
  (newTape, desc)

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
  let t = try Some (Yojson.Basic.from_file name) with | _ -> Failure "json file error" in
  let getTran (x : Yojson.Basic.json) =
    match x with
    | (`Assoc y ) -> getTransitions y
    | _ -> raise (Parsing_error "Error while parsing, no transitions.")
  in
  match t with
  | Failure e -> Failure e
  | Some t ->
    begin 
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
    end

let search_alphabet_opt alphabet letter =
  List.find_opt (fun x -> (x = letter)) alphabet

let search_alphabet alphabet letter =
  match List.find (fun x -> (x = letter)) alphabet with
  | _ -> ()

let search_state_opt s states =
  List.find_opt (fun y -> y = s) states

let search_state s states =
  match List.find (fun y -> y = s) states with
  | _ -> ()

let check_description d =
  try match search_alphabet_opt d.alphabet d.blank with
    | Some _ ->
      begin
        begin
          try match search_state d.initial d.states with | _ -> () with
          | _ -> raise (Parsing_error "Error initial state not valid")
        end;
        begin
          try List.iter (fun x ->
              match search_state x d.states with
              | _ -> ()) d.finals
          with
          | _ -> raise (Parsing_error "Error finals state not valid")
        end
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
      | Failure x -> Failure x
      | _ ->
        if (check_input input description.blank)
        then Some ((Tape.tape_of_list description.blank (list_from_string input)), description)
        else Failure "Error there is blank in input."
    end

let (^$) c s = (Char.escaped c) ^ s
let ($^) s c = s ^ (Char.escaped c)

let printDescription (_, description) =
  let rec get_alphabet = function
    | l :: [] -> Char.escaped l
    | l :: next -> (Char.escaped l) ^ ", " ^ (get_alphabet next)
    | [] -> ""
  in
  let rec get_strs = function
    | s :: [] -> s
    | s :: next -> (get_strs next) ^ ", " ^ s
    | [] -> ""
  in
  print_endline ("Name: " ^ description.name);
  print_endline ("Alphabet: [" ^ (get_alphabet description.alphabet) ^ "]");
  print_endline ("States  : [" ^ get_strs description.states ^ "]");
  print_endline ("Initial : " ^ description.initial);
  print_endline ("Finals  : " ^ get_strs description.finals);
  StateMap.iter (fun sk sv -> CharMap.iter (fun ck (ns, nc, mov) -> print_endline ("(" ^ sk ^ ", " ^ ck ^$ ") -> (" ^ ns ^ ", " ^ nc ^$ ", " ^ mov ^ ")")) sv) description.transitions

let compute (tape, description) =
  let computeState state tape =
    try
      begin
        match (CharMap.find (Tape.current tape) (StateMap.find state description.transitions)) with
        | (to_state, write, action) -> print_endline ("(" ^ state ^ ", " ^ Tape.current tape ^$ ") -> (" ^ to_state ^ ", " ^ write ^$ ", " ^ action ^ ")"); Some (to_state, (
            match action with
            | "LEFT" -> let t = Tape.prev description.blank (Tape.newCurrent tape write) in Tape.print t 10 description.blank ; t
            | "RIGHT" -> let t = Tape.next description.blank (Tape.newCurrent tape write) in Tape.print t 10 description.blank; t
            | _ -> raise Error
          ))
      end
    with
    | _ -> Failure "Error"
  in
  let rec computing tape current_state =
    match search_state_opt current_state description.finals with
    | None ->
      begin
        match (computeState current_state tape) with
        | Some (next_state, newTape) -> computing newTape next_state
        | Failure e -> print_endline e
      end
    | _ -> ()
  in
  Tape.print tape 10 description.blank;
  computing tape description.initial

module Complexity =
struct
  let generate_add x = "1+" ^ (String.init x (fun _ -> '1')) ^ "="

  let generate_function jsonfile = 
    match jsonfile with
    | "unary_add.json" -> Some generate_add
    | _ -> Failure "impossible to calculate time complexity for this description"

  let complexity_of_machine (tape, description) =
    let computeState state tape =
      try
        begin
          match (CharMap.find (Tape.current tape) (StateMap.find state description.transitions)) with
          | (to_state, write, action) -> Some (to_state, (
              match action with
              | "LEFT" -> Tape.prev description.blank (Tape.newCurrent tape write)
              | "RIGHT" -> Tape.next description.blank (Tape.newCurrent tape write)
              | _ -> raise Error
            ))
        end
      with
      | _ -> Failure "Error"
    in
    let rec computing tape current_state acc =
      match search_state_opt current_state description.finals with
      | None ->
        begin
          match (computeState current_state tape) with
          | Some (next_state, newTape) -> computing newTape next_state (acc + 1)
          | Failure e -> 0
        end
      | _ -> acc
    in
    computing tape description.initial 0

  let init () =
    Graphics.open_graph " 800x600+0-0";
    Graphics.set_window_title "Complexity of description";
    Graphics.moveto 300 500; Graphics.set_font "-misc-dejavu sans mono-bold-r-normal--13-0-0-0-m-0-iso8859-1";
    Graphics.draw_string "Big O Complexity";
    Graphics.moveto 100 100; Graphics.lineto 100 450;
    Graphics.moveto 100 100; Graphics.lineto 600 100

  let display lst =
    init ();
    ignore(Graphics.read_key ());
    Graphics.close_graph ()

  let compute jsonfile =
    match (generate_function jsonfile) with
    | Failure e -> print_endline e
    | Some gen_f ->
      begin
        match (getMachine jsonfile (gen_f 0)) with
        | Failure e -> print_endline e
        | Some m ->
          let intList = List.init 101 (fun x -> complexity_of_machine (changeTape m (gen_f x))) in
          display intList
      end
end
