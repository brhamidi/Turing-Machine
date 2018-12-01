(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   turing.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: bhamidi <marvin@42.fr>                     +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/11/14 16:37:19 by bhamidi           #+#    #+#             *)
(*   Updated: 2018/11/30 17:28:08 by msrun            ###   ########.fr       *)
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
  let generate_02n x = String.init x (fun _ -> '0') ^ "="
  let generate_0n1n x = String.init x (fun _ -> '0') ^ String.init x (fun _ -> '1') ^ "="
  let generate_add x = "1+" ^ (String.init x (fun _ -> '1')) ^ "="
  let generate_sub x = (String.init x (fun _ -> '1')) ^ "-" ^ (String.init x (fun _ -> '1')) ^ "="
  let generate_palindrome x = String.init x (fun _ -> '0') ^ String.init x (fun _ -> '1') ^ String.init x (fun _ -> '0') ^ "="

  let generate_function jsonfile = 
    match jsonfile with
    | "unary_add.json" -> Some generate_add
    | "unary_sub.json" -> Some generate_sub
    | "unary_02n.json" -> Some generate_02n
    | "unary_0n1n.json" -> Some generate_0n1n
    | "unary_palindrome.json" -> Some generate_palindrome
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

  let rec power x e =
    if e = 1 then x
    else if e = 0 then 1
    else x * (power x (e - 1))

  let init filename =
    Graphics.open_graph " 1300x800+0-0";
    Graphics.set_window_title "Complexity of description";
    Graphics.moveto 600 700; 
    Graphics.draw_string "Big O Complexity for "; Graphics.draw_string filename; Graphics.draw_string " description";
    Graphics.set_line_width 2;
    Graphics.moveto 100 100; Graphics.lineto 100 600;
    Graphics.moveto 100 100; Graphics.lineto 1100 100;
    Graphics.moveto 1060 45 ; Graphics.draw_string "element";
    Graphics.moveto 20 620 ; Graphics.draw_string "operation";
    List.iter (fun x -> Graphics.moveto (100 + (x * 10)) 80;Graphics.draw_string (string_of_int x)) (List.init 11 (fun x -> x * 10));
    List.iter (fun x -> Graphics.moveto 75 (100 + (x / 2));Graphics.draw_string (string_of_int x)) (List.init 11 (fun x -> x * 100))

  let print_curv intList =
    let lst = List.init 101 (fun x -> x) in
    let putX = fun x -> 100 + (x * 10) in
    let putY = fun f x -> (100 + ((f x) / 2)) in
    let nComplexity = fun n -> n in
    let n2Complexity = fun n -> n * n in
    let n2nComplexity = fun n -> if n = 10 then 1000 else if n > 10 then 2000 else power 2 n in
    let nlogn = fun x -> let floatx = float_of_int x in int_of_float (floatx *. log(floatx)) in
    let rec fact n = if n = 7 then 1000 else if n > 7 then 2000 else if n = 0 then 1 else n * fact (n -1) in
    let getFonction fn = fun x -> let y = putY fn x in if y < 601 then Graphics.lineto (putX x) y in
    let printO str = Graphics.set_color Graphics.black; Graphics.draw_string str in
    Graphics.set_line_width 4;
    Graphics.set_color Graphics.cyan; Graphics.moveto 100 100; List.iter (getFonction nComplexity) lst;
    Graphics.moveto 1150 600; Graphics.lineto 1190 600; printO " O(n)";
    Graphics.set_color Graphics.magenta; Graphics.moveto 100 100; List.iter (getFonction nlogn) lst;
    Graphics.moveto 1150 580; Graphics.lineto 1190 580; printO " O(nlogn)";
    Graphics.set_color Graphics.green; Graphics.moveto 100 100; List.iter (getFonction n2Complexity) lst;
    Graphics.moveto 1150 560; Graphics.lineto 1190 560; printO " O(n^2)";
    Graphics.set_color Graphics.red; Graphics.moveto 100 100; List.iter (getFonction n2nComplexity) lst;
    Graphics.moveto 1150 540; Graphics.lineto 1190 540; printO " O(2^n)";
    Graphics.set_color Graphics.blue; Graphics.moveto 100 100; List.iter (getFonction fact) lst;
    Graphics.moveto 1150 520; Graphics.lineto 1190 520; printO " O(!n)";

    (* description curve *)
    Graphics.set_color Graphics.black; Graphics.moveto 100 100;
    List.iteri (fun i x -> let y = putY (fun x -> x) x in if y < 601 then Graphics.lineto (putX i) y) intList;
    Graphics.moveto 1150 480; Graphics.lineto 1190 480; printO " Your Curve"

  let display lst filename =
    init filename;
    print_curv lst;
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
          display intList jsonfile
      end
end
