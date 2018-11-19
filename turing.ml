(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   turing.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: bhamidi <marvin@42.fr>                     +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/11/14 16:37:19 by bhamidi           #+#    #+#             *)
(*   Updated: 2018/11/19 16:04:46 by msrun            ###   ########.fr       *)
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
      CharMap.add (String.get a 0) ( b, String.get c 0, d) x
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
    | _ -> raise (Parsing_error "Error while parsing alphabet field") in
  List.fold_left (fun x y -> (match y with | `String x -> String.get x 0 | _ -> 'r') :: x) [] lA

let getBlank str =
  match str with
  | `String s -> String.get s 0
  | _ -> raise (Parsing_error "Error while parsing blank field.")

let getListStr lstr =
  let lS = match lstr with | `List l -> l | _ -> raise (Parsing_error "Error while parsing list of string") in
  List.fold_left (fun x y -> (match y with | `String x -> x | _ -> raise (Parsing_error "Error while parsing list of string")) :: x) [] lS

let callFunction f field json =
  f (Yojson.Basic.Util.member field json)

let getDescrition name : descriptions trying =
  let t = Yojson.Basic.from_file name in
  let getTran (x : Yojson.Basic.json) =
    match x with
    | (`Assoc y ) -> getTransitions y
    | _ -> StateMap.empty
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

let getMachine jsonfile input : t trying =
  let desc = getDescrition jsonfile in
  match desc with
  | Failure e -> Failure e
  | Some description ->
    Some (Tape.tape_of_list ['1';'1';'1';'-';'1';'='], description)
