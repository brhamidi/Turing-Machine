(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   turing.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: bhamidi <marvin@42.fr>                     +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/11/14 16:37:19 by bhamidi           #+#    #+#             *)
(*   Updated: 2018/11/15 18:00:40 by msrun            ###   ########.fr       *)
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

type t = Tape.t * descriptions

let getTransitions l =
  let f x y = match y with
    | `Assoc [("read", `String a);("to_state", `String b);("write", `String c);("action", `String d)] -> CharMap.add (String.get a 0) ( b, String.get c 0, d) x
    | _ -> print_endline "Error: transition not valid"; CharMap.add '0' ("REJECTED", '0', "ERROR") CharMap.empty
  in
  let fillCmap l = List.fold_left (fun x y -> f x y) CharMap.empty l
  in
  let fillSmap l = List.fold_left (fun x y -> match y with | (transi, lt) -> StateMap.add transi (fillCmap ( match lt with | `List x -> x | _ -> [])) x) StateMap.empty l
  in
  fillSmap l

let getStr str =
  match str with
  | `String s -> s
  | _ -> "Error"

let getAlphabet l =
  let lA = match l with | `List l -> l | _ -> [] in
  List.fold_left (fun x y -> (match y with | `String x -> String.get x 0 | _ -> 'r') :: x) [] lA

let getBlank str =
  match str with
  | `String s -> String.get s 0
  | _ -> 'r'

let getListStr lstr =
  let lS = match lstr with | `List l -> l | _ -> [] in
  List.fold_left (fun x y -> (match y with | `String x -> x | _ -> "") :: x) [] lS


let fromFile name : descriptions =
  let t = Yojson.Basic.from_file name in
  let f1 (x : Yojson.Basic.json) = match x with
    | (`Assoc y ) -> getTransitions y
    | _ -> StateMap.empty
  in
  {
    name = getStr (Yojson.Basic.Util.member "names" t);
    alphabet = getAlphabet (Yojson.Basic.Util.member "alphabet" t);
    blank = getBlank (Yojson.Basic.Util.member "blank" t);
    states = getListStr (Yojson.Basic.Util.member "states" t);
    initial = getStr (Yojson.Basic.Util.member "initial" t);
    finals = getListStr (Yojson.Basic.Util.member "finals" t);
    transitions = f1 (Yojson.Basic.Util.member "transitions" t)
  }

