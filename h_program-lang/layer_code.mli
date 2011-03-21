
(* The filenames in this data structure are in readable format
 * so one can use the layer generated by another user on
 * his own repository (this also saves some space in the generated
 * JSON file). 
 *)
type layer = {
  title: string;
  description: string;
  files: (Common.filename * file_info) list;
  kinds: (kind * Simple_color.emacs_color) list;
 }
 and file_info = {
   micro_level: (int (* line *) * kind) list;
   macro_level: (kind * float (* percentage of rectangle *)) list;
 }
 (* ugly: the first letter of the propery cannot be in uppercase because
  * of the ugly way Ocaml.json_of_v currently works.
  *)
 and kind = string

(* The filenames are in absolute path format in the index. *)
type layers_with_index = {
  root: Common.dirname;
  layers: (layer * bool (* is active *)) list;

  micro_index:
    (Common.filename, (int, Simple_color.emacs_color) Hashtbl.t) Hashtbl.t;
  macro_index:
    (Common.filename, (float * Simple_color.emacs_color) list) Hashtbl.t;
}

val build_index_of_layers: 
  root:Common.dirname -> 
  (layer * bool) list -> 
  layers_with_index
val has_active_layers: layers_with_index -> bool

(* save either in a (readable) json format or (fast) marshalled form 
 * depending on the extension of the filename
 *)
val load_layer: Common.filename -> layer
val save_layer: layer -> Common.filename -> unit

(* helpers *)
val json_of_layer: layer -> Json_type.t
val layer_of_json: Json_type.t -> layer

val simple_layer_of_parse_infos: 
  root:Common.dirname ->
  title:string ->
  ?description:string ->
  (Parse_info.info * kind) list ->
  (kind * Simple_color.emacs_color) list ->
  layer

val stat_of_layer: layer -> (kind * int) list

val filter_layer: (Common.filename -> bool) -> layer -> layer
