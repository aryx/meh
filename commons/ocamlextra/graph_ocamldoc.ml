(***********************************************************************)
(*                               OCamldoc                              *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* pad: a part of ocaml/ocamldoc/odop_dep.ml *)

type id = string

module S = Set.Make (struct type t = string let compare = compare end)

let set_to_list s =
  let l = ref [] in
  S.iter (fun e -> l := e :: !l) s;
  !l

type node = {
    id : id ;
    mutable near : S.t ; (** fils directs *)
    mutable far : (id * S.t) list ; (** fils indirects, par quel fils *)
    reflex : bool ; (** reflexive or not, we keep 
                       information here to remove the node itself from its direct children *)
  }

type graph = node list

let make_node s children =
  let set = List.fold_right
      S.add
      children
      S.empty 
  in
  { id = s; 
    near = S.remove s set ;
    far = [] ;
    reflex = List.mem s children ;
  }

let get_node graph s = 
  try List.find (fun n -> n.id = s) graph
  with Not_found ->  
    make_node s []

let rec trans_closure graph acc n =
  if S.mem n.id acc then
    acc
  else
    (* optimisation plus tard : utiliser le champ far si non vide ? *)
    S.fold
      (fun child -> fun acc2 ->
        trans_closure graph acc2 (get_node graph child))
      n.near
      (S.add n.id acc)
  
let node_trans_closure graph n =
  let far = List.map
      (fun child -> 
        let set = trans_closure graph S.empty (get_node graph child) in
        (child, set)
      )
      (set_to_list n.near)
  in
  n.far <- far

let compute_trans_closure graph =
  List.iter (node_trans_closure graph) graph

let prune_node graph node =
  S.iter
    (fun child ->
      let set_reachables = List.fold_left
          (fun acc -> fun (ch, reachables) ->
            if child = ch then
              acc
            else
              S.union acc reachables
          )
          S.empty
          node.far
      in
      let set = S.remove node.id set_reachables in
      if S.exists (fun n2 -> S.mem child (get_node graph n2).near) set then
        (
         node.near <- S.remove child node.near ;
         node.far <- List.filter (fun (ch,_) -> ch <> child) node.far
        )
      else
        ()
    )
    node.near;
  if node.reflex then 
    node.near <- S.add node.id node.near 
  else
    ()

let kernel graph =
  (* compute transitive closure *)
  compute_trans_closure graph ;

  (* remove edges to keep a transitive kernel *)
  List.iter (prune_node graph) graph;

  graph



