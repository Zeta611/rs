module Ident = struct
  type t = { stamp : int; name : string; mutable flags : int }
end

module Lexing = struct
  type position = {
    pos_fname : string;
    pos_lnum : int;
    pos_bol : int;
    pos_cnum : int;
  }
end

module Location = struct
  open Lexing

  type t = { loc_start : position; loc_end : position; loc_ghost : bool }
  type 'a loc = { txt : 'a; loc : t }
end

module Path = struct
  type t = Pident of Ident.t | Pdot of t * string * int | Papply of t * t
end

module Misc = struct
  exception Fatal_error

  let fatal_error msg =
    prerr_string ">> Fatal error: ";
    prerr_endline msg;
    raise Fatal_error
end

module Identifiable = struct
  module type Thing = sig
    type t

    include Hashtbl.HashedType with type t := t
    include Map.OrderedType with type t := t

    val output : out_channel -> t -> unit
    val print : Format.formatter -> t -> unit
  end

  module type Set = sig
    module T : Set.OrderedType
    include Set.S with type elt = T.t and type t = Set.Make(T).t

    val output : out_channel -> t -> unit
    val print : Format.formatter -> t -> unit
    val to_string : t -> string
    val of_list : elt list -> t
    val map : (elt -> elt) -> t -> t
  end

  module type Map = sig
    module T : Map.OrderedType
    include Map.S with type key = T.t and type 'a t = 'a Map.Make(T).t

    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val of_list : (key * 'a) list -> 'a t

    val disjoint_union :
      ?eq:('a -> 'a -> bool) ->
      ?print:(Format.formatter -> 'a -> unit) ->
      'a t ->
      'a t ->
      'a t

    val union_right : 'a t -> 'a t -> 'a t
    val union_left : 'a t -> 'a t -> 'a t
    val union_merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val rename : key t -> key -> key
    val map_keys : (key -> key) -> 'a t -> 'a t
    val keys : 'a t -> Set.Make(T).t
    val data : 'a t -> 'a list
    val of_set : (key -> 'a) -> Set.Make(T).t -> 'a t
    val transpose_keys_and_data : key t -> key t
    val transpose_keys_and_data_set : key t -> Set.Make(T).t t

    val print :
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  end

  module type Tbl = sig
    module T : sig
      type t

      include Map.OrderedType with type t := t
      include Hashtbl.HashedType with type t := t
    end

    include Hashtbl.S with type key = T.t and type 'a t = 'a Hashtbl.Make(T).t

    val to_list : 'a t -> (T.t * 'a) list
    val of_list : (T.t * 'a) list -> 'a t
    val to_map : 'a t -> 'a Map.Make(T).t
    val of_map : 'a Map.Make(T).t -> 'a t
    val memoize : 'a t -> (key -> 'a) -> key -> 'a
    val map : 'a t -> ('a -> 'b) -> 'b t
  end

  module Make_map (T : Thing) = struct
    include Map.Make (T)

    let filter_map f t =
      fold
        (fun id v map ->
          match f id v with None -> map | Some r -> add id r map)
        t empty

    let of_list l = List.fold_left (fun map (id, v) -> add id v map) empty l

    let disjoint_union ?eq ?print m1 m2 =
      union
        (fun id v1 v2 ->
          let ok = match eq with None -> false | Some eq -> eq v1 v2 in
          if not ok then
            let err =
              match print with
              | None -> Format.asprintf "Map.disjoint_union %a" T.print id
              | Some print ->
                  Format.asprintf "Map.disjoint_union %a => %a <> %a" T.print id
                    print v1 print v2
            in
            Misc.fatal_error err
          else Some v1)
        m1 m2

    let union_right m1 m2 =
      merge
        (fun _id x y ->
          match (x, y) with
          | None, None -> None
          | None, Some v | Some v, None | Some _, Some v -> Some v)
        m1 m2

    let union_left m1 m2 = union_right m2 m1

    let union_merge f m1 m2 =
      let aux _ m1 m2 =
        match (m1, m2) with
        | None, m | m, None -> m
        | Some m1, Some m2 -> Some (f m1 m2)
      in
      merge aux m1 m2

    let rename m v = try find v m with Not_found -> v
    let map_keys f m = of_list (List.map (fun (k, v) -> (f k, v)) (bindings m))

    let print f ppf s =
      let elts ppf s =
        iter (fun id v -> Format.fprintf ppf "@ (@[%a@ %a@])" T.print id f v) s
      in
      Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s

    module T_set = Set.Make (T)

    let keys map = fold (fun k _ set -> T_set.add k set) map T_set.empty
    let data t = List.map snd (bindings t)
    let of_set f set = T_set.fold (fun e map -> add e (f e) map) set empty
    let transpose_keys_and_data map = fold (fun k v m -> add v k m) map empty

    let transpose_keys_and_data_set map =
      fold
        (fun k v m ->
          let set =
            match find v m with
            | exception Not_found -> T_set.singleton k
            | set -> T_set.add k set
          in
          add v set m)
        map empty
  end

  module Make_set (T : Thing) = struct
    include Set.Make (T)

    let output oc s =
      Printf.fprintf oc " ( ";
      iter (fun v -> Printf.fprintf oc "%a " T.output v) s;
      Printf.fprintf oc ")"

    let print ppf s =
      let elts ppf s = iter (fun e -> Format.fprintf ppf "@ %a" T.print e) s in
      Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s

    let to_string s = Format.asprintf "%a" print s

    let of_list l =
      match l with
      | [] -> empty
      | [ t ] -> singleton t
      | t :: q -> List.fold_left (fun acc e -> add e acc) (singleton t) q

    let map f s = of_list (List.map f (elements s))
  end

  module Make_tbl (T : Thing) = struct
    include Hashtbl.Make (T)
    module T_map = Make_map (T)

    let to_list t = fold (fun key datum elts -> (key, datum) :: elts) t []

    let of_list elts =
      let t = create 42 in
      List.iter (fun (key, datum) -> add t key datum) elts;
      t

    let to_map v = fold T_map.add v T_map.empty

    let of_map m =
      let t = create (T_map.cardinal m) in
      T_map.iter (fun k v -> add t k v) m;
      t

    let memoize t f key =
      try find t key
      with Not_found ->
        let r = f key in
        add t key r;
        r

    let map t f = of_map (T_map.map f (to_map t))
  end

  module type S = sig
    type t

    module T : Thing with type t = t
    include Thing with type t := T.t
    module Set : Set with module T := T
    module Map : Map with module T := T
    module Tbl : Tbl with module T := T
  end

  module Make (T : Thing) : S with type t := T.t = struct
    module T = T
    include T
    module Set = Make_set (T)
    module Map = Make_map (T)
    module Tbl = Make_tbl (T)
  end
end

module Ident = struct
  open Format

  type t = { stamp : int; name : string; mutable flags : int }

  let global_flag = 1
  let unique_name i = i.name ^ "_" ^ string_of_int i.stamp
  let equal i1 i2 = i1.name = i2.name
  let same i1 i2 = i1 = i2
  (* Possibly more efficient version (with a real compiler, at least):
       if i1.stamp <> 0
       then i1.stamp = i2.stamp
       else i2.stamp = 0 && i1.name = i2.name *)

  let compare x y =
    let c = x.stamp - y.stamp in
    if c <> 0 then c
    else
      let c = compare x.name y.name in
      if c <> 0 then c else compare x.flags y.flags

  let global i = i.flags land global_flag <> 0

  let print ppf i =
    match i.stamp with
    | 0 -> fprintf ppf "%s!" i.name
    | -1 -> fprintf ppf "%s#" i.name
    | n -> fprintf ppf "%s/%i%s" i.name n (if global i then "g" else "")

  let output oc id = output_string oc (unique_name id)
  let hash i = Char.code i.name.[0] lxor i.stamp
  let original_equal = equal

  include Identifiable.Make (struct
    type nonrec t = t

    let compare = compare
    let output = output
    let print = print
    let hash = hash
    let equal = same
  end)

  let equal = original_equal
end

module Longident = struct
  type t = Lident of string | Ldot of t * string | Lapply of t * t
end

module Asttypes = struct
  type constant =
    | Const_int of int
    | Const_char of char
    | Const_string of string * string option
    | Const_float of string
    | Const_int32 of int32
    | Const_int64 of int64
    | Const_nativeint of nativeint

  type rec_flag = Nonrecursive | Recursive
  type direction_flag = Upto | Downto

  (* Order matters, used in polymorphic comparison *)

  type private_flag = Private | Public
  type mutable_flag = Immutable | Mutable
  type override_flag = Override | Fresh
  type closed_flag = Closed | Open
  type label = string

  type arg_label =
    | Nolabel
    | Labelled of string (*  label:T -> ... *)
    | Optional of string (* ?label:T -> ... *)

  type 'a loc = 'a Location.loc = { txt : 'a; loc : Location.t }
end

module Parsetree = struct
  open Asttypes

  type attribute = string loc (* * payload *)
  and extension = string loc

  (* * payload *)
  (* [%id ARG]
     [%%id ARG]

     Sub-language placeholder -- rejected by the typechecker.
  *)
  and attributes = attribute list
  (* Type expressions *)

  (** {1 Core language} *)

  and core_type = {
    ptyp_desc : core_type_desc;
    ptyp_loc : Location.t;
    ptyp_attributes : attributes; (* ... [@id1] [@id2] *)
  }

  and core_type_desc =
    | Ptyp_any (*  _ *)
    | Ptyp_var of string (* 'a *)
    | Ptyp_arrow of arg_label * core_type * core_type
      (* T1 -> T2       Simple
         ~l:T1 -> T2    Labelled
         ?l:T1 -> T2    Optional
      *)
    | Ptyp_tuple of core_type list
      (* T1 * ... * Tn

         Invariant: n >= 2
      *)
    | Ptyp_constr of Longident.t loc * core_type list
    (* tconstr
       T tconstr
       (T1, ..., Tn) tconstr
    *)
    (* | Ptyp_object of object_field list * closed_flag *)
    (* < l1:T1; ...; ln:Tn >     (flag = Closed)
       < l1:T1; ...; ln:Tn; .. > (flag = Open)
    *)
    | Ptyp_class of Longident.t loc * core_type list
      (* #tconstr
         T #tconstr
         (T1, ..., Tn) #tconstr
      *)
    | Ptyp_alias of core_type * string (* T as 'a *)
    | Ptyp_variant of row_field list * closed_flag * label list option
      (* [ `A|`B ]         (flag = Closed; labels = None)
         [> `A|`B ]        (flag = Open;   labels = None)
         [< `A|`B ]        (flag = Closed; labels = Some [])
         [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
      *)
    | Ptyp_poly of string loc list * core_type
      (* 'a1 ... 'an. T

         Can only appear in the following context:

         - As the core_type of a Ppat_constraint node corresponding
           to a constraint on a let-binding: let x : 'a1 ... 'an. T
           = e ...

         - Under Cfk_virtual for methods (not values).

         - As the core_type of a Pctf_method node.

         - As the core_type of a Pexp_poly node.

         - As the pld_type field of a label_declaration.

         - As a core_type of a Ptyp_object node.
      *)
    | Ptyp_package of package_type (* (module S) *)
    | Ptyp_extension of extension
  (* [%id] *)

  and package_type = Longident.t loc * (Longident.t loc * core_type) list
  (*
        (module S)
        (module S with type t1 = T1 and ... and tn = Tn)
       *)

  and row_field =
    | Rtag of label loc * attributes * bool * core_type list
      (* [`A]                   ( true,  [] )
          [`A of T]              ( false, [T] )
          [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
          [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )

         - The 2nd field is true if the tag contains a
           constant (empty) constructor.
         - '&' occurs when several types are used for the same constructor
           (see 4.2 in the manual)

         - TODO: switch to a record representation, and keep location
      *)
    | Rinherit of core_type
  (* [ T ] *)

  (* Patterns *)
  and pattern = {
    ppat_desc : pattern_desc;
    ppat_loc : Location.t;
    ppat_attributes : attributes; (* ... [@id1] [@id2] *)
  }

  and pattern_desc =
    | Ppat_any (* _ *)
    | Ppat_var of string loc (* x *)
    | Ppat_alias of pattern * string loc (* P as 'a *)
    | Ppat_constant of constant (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
    | Ppat_interval of constant * constant
      (* 'a'..'z'

         Other forms of interval are recognized by the parser
         but rejected by the type-checker. *)
    | Ppat_tuple of pattern list
      (* (P1, ..., Pn)

         Invariant: n >= 2
      *)
    | Ppat_construct of Longident.t loc * pattern option
      (* C                None
         C P              Some P
         C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
      *)
    | Ppat_variant of label * pattern option
      (* `A             (None)
         `A P           (Some P)
      *)
    | Ppat_record of (Longident.t loc * pattern) list * closed_flag
      (* { l1=P1; ...; ln=Pn }     (flag = Closed)
         { l1=P1; ...; ln=Pn; _}   (flag = Open)

         Invariant: n > 0
      *)
    | Ppat_array of pattern list (* [| P1; ...; Pn |] *)
    | Ppat_or of pattern * pattern (* P1 | P2 *)
    | Ppat_constraint of pattern * core_type (* (P : T) *)
    | Ppat_type of Longident.t loc (* #tconst *)
    | Ppat_lazy of pattern (* lazy P *)
    | Ppat_unpack of string loc
      (* (module P)
         Note: (module P : S) is represented as
         Ppat_constraint(Ppat_unpack, Ptyp_package)
      *)
    | Ppat_exception of pattern (* exception P *)
    | Ppat_extension of extension (* [%id] *)
    | Ppat_open of Longident.t loc * pattern
  (* M.(P) *)
end

module Primitive = struct
  type native_repr = Same_as_ocaml_repr

  type description = {
    prim_name : string; (* Name of primitive  or C function *)
    prim_arity : int; (* Number of arguments *)
    prim_alloc : bool; (* Does it allocates or raise? *)
    prim_native_name : string; (* Name of C function for the nat. code gen. *)
    prim_native_repr_args : native_repr list;
    prim_native_repr_res : native_repr;
  }
end

module Types = struct
  open Asttypes

  type label = string

  type type_expr = { mutable desc : type_desc; mutable level : int; id : int }

  and type_desc =
    | Tvar of string option
    | Tarrow of arg_label * type_expr * type_expr * commutable
    | Ttuple of type_expr list
    | Tconstr of Path.t * type_expr list * abbrev_memo ref
    | Tobject of type_expr * (Path.t * type_expr list) option ref
    | Tfield of string * field_kind * type_expr * type_expr
    | Tnil
    | Tlink of type_expr
    | Tsubst of type_expr (* for copying *)
    | Tvariant of row_desc
    | Tunivar of string option
    | Tpoly of type_expr * type_expr list
    | Tpackage of Path.t * Longident.t list * type_expr list

  and row_desc = {
    row_fields : (label * row_field) list;
    row_more : type_expr;
    row_bound : unit;
    row_closed : bool;
    row_fixed : bool;
    row_name : (Path.t * type_expr list) option;
  }

  and row_field =
    | Rpresent of type_expr option
    | Reither of bool * type_expr list * bool * row_field option ref
    (* 1st true denotes a constant constructor *)
    (* 2nd true denotes a tag in a pattern matching, and
       is erased later *)
    | Rabsent

  and abbrev_memo =
    | Mnil
    | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
    | Mlink of abbrev_memo ref

  and field_kind = Fvar of field_kind option ref | Fpresent | Fabsent
  and commutable = Cok | Cunknown | Clink of commutable ref

  (* Value descriptions *)

  type value_description = {
    val_type : type_expr; (* Type of the value *)
    val_kind : value_kind;
    val_loc : Location.t;
    val_attributes : Parsetree.attributes;
  }

  and value_kind =
    | Val_reg (* Regular value *)
    | Val_prim of Primitive.description
  (* Primitive *)
  (* Variance *)

  module Variance = struct
    type t = int
    type f = May_pos | May_neg | May_weak | Inj | Pos | Neg | Inv

    let single = function
      | May_pos -> 1
      | May_neg -> 2
      | May_weak -> 4
      | Inj -> 8
      | Pos -> 16
      | Neg -> 32
      | Inv -> 64

    let union v1 v2 = v1 lor v2
    let inter v1 v2 = v1 land v2
    let subset v1 v2 = v1 land v2 = v1
    let set x b v = if b then v lor single x else v land lnot (single x)
    let mem x = subset (single x)
    let null = 0
    let may_inv = 7
    let full = 127
    let covariant = single May_pos lor single Pos lor single Inj

    let swap f1 f2 v =
      let v' = set f1 (mem f2 v) v in
      set f2 (mem f1 v) v'

    let conjugate v = swap May_pos May_neg (swap Pos Neg v)
    let get_upper v = (mem May_pos v, mem May_neg v)
    let get_lower v = (mem Pos v, mem Neg v, mem Inv v, mem Inj v)
  end

  (* Type definitions *)

  type type_declaration = {
    type_params : type_expr list;
    type_arity : int;
    type_kind : type_kind;
    type_private : private_flag;
    type_manifest : type_expr option;
    type_variance : Variance.t list;
    type_newtype_level : (int * int) option;
    type_loc : Location.t;
    type_attributes : Parsetree.attributes;
    type_immediate : bool;
    type_unboxed : unboxed_status;
  }

  and type_kind =
    | Type_abstract
    | Type_record of label_declaration list * record_representation
    | Type_variant of constructor_declaration list
    | Type_open

  and record_representation =
    | Record_regular (* All fields are boxed / tagged *)
    | Record_float (* All fields are floats *)
    | Record_unboxed of bool (* Unboxed single-field record, inlined or not *)
    | Record_inlined of { tag : int; name : string; num_nonconsts : int }
      (* Inlined record *)
    | Record_extension (* Inlined record under extension *)

  and label_declaration = {
    ld_id : Ident.t;
    ld_mutable : mutable_flag;
    ld_type : type_expr;
    ld_loc : Location.t;
    ld_attributes : Parsetree.attributes;
  }

  and constructor_declaration = {
    cd_id : Ident.t;
    cd_args : constructor_arguments;
    cd_res : type_expr option;
    cd_loc : Location.t;
    cd_attributes : Parsetree.attributes;
  }

  and constructor_arguments =
    | Cstr_tuple of type_expr list
    | Cstr_record of label_declaration list

  and unboxed_status = {
    unboxed : bool;
    default : bool; (* False if the unboxed field was set from an attribute. *)
  }

  (* Constructor and record label descriptions inserted held in typing
     environments *)

  type constructor_description = {
    cstr_name : string; (* Constructor name *)
    cstr_res : type_expr; (* Type of the result *)
    cstr_existentials : type_expr list; (* list of existentials *)
    cstr_args : type_expr list; (* Type of the arguments *)
    cstr_arity : int; (* Number of arguments *)
    cstr_tag : constructor_tag; (* Tag for heap blocks *)
    cstr_consts : int; (* Number of constant constructors *)
    cstr_nonconsts : int; (* Number of non-const constructors *)
    cstr_normal : int; (* Number of non generalized constrs *)
    cstr_generalized : bool; (* Constrained return type? *)
    cstr_private : private_flag; (* Read-only constructor? *)
    cstr_loc : Location.t;
    cstr_attributes : Parsetree.attributes;
    cstr_inlined : type_declaration option;
  }

  and constructor_tag =
    | Cstr_constant of int (* Constant constructor (an int) *)
    | Cstr_block of int (* Regular constructor (a block) *)
    | Cstr_unboxed (* Constructor of an unboxed type *)
    | Cstr_extension of Path.t * bool
  (* Extension constructor     true if a constant false if a block*)

  type label_description = {
    lbl_name : string; (* Short name *)
    lbl_res : type_expr; (* Type of the result *)
    lbl_arg : type_expr; (* Type of the argument *)
    lbl_mut : mutable_flag; (* Is this a mutable field? *)
    lbl_pos : int; (* Position in block *)
    lbl_all : label_description array; (* All the labels in this type *)
    lbl_repres : record_representation; (* Representation for this record *)
    lbl_private : private_flag; (* Read-only field? *)
    lbl_loc : Location.t;
    lbl_attributes : Parsetree.attributes;
  }
end

module Env = struct
  type t = {
    (* values : value_description IdTbl.t;
     * constrs : constructor_description TycompTbl.t;
     * labels : label_description TycompTbl.t;
     * types : (type_declaration * type_descriptions) IdTbl.t;
     * modules :
     *   (Subst.t * module_declaration, module_declaration) EnvLazy.t IdTbl.t;
     * modtypes : modtype_declaration IdTbl.t;
     * components : module_components IdTbl.t;
     * classes : class_declaration IdTbl.t;
     * cltypes : class_type_declaration IdTbl.t;
     * functor_args : unit Ident.tbl;
     * summary : summary;
     * local_constraints : type_declaration PathMap.t;
     * gadt_instances : (int * TypeSet.t ref) list; *)
    flags : int;
  }
end

module Typedtree = struct
  open Asttypes
  open Types

  (* Value expressions for the core language *)

  type partial = Partial | Total
  type attribute = Parsetree.attribute
  type attributes = attribute list

  type pattern = {
    pat_desc : pattern_desc;
    pat_loc : Location.t;
    (* pat_extra : (pat_extra * Location.t * attribute list) list; *)
    pat_type : type_expr;
    (* mutable pat_env : Env.t; *)
    pat_attributes : attribute list;
  }

  (* and pat_extra =
   *   | Tpat_constraint of core_type
   *   | Tpat_type of Path.t * Longident.t loc
   *   | Tpat_open of Path.t * Longident.t loc * Env.t
   *   | Tpat_unpack *)
  and pattern_desc =
    | Tpat_any
    | Tpat_var of Ident.t * string loc
    | Tpat_alias of pattern * Ident.t * string loc
    | Tpat_constant of constant
    | Tpat_tuple of pattern list
    | Tpat_construct of Longident.t loc * constructor_description * pattern list
    | Tpat_variant of label * pattern option * row_desc ref
    | Tpat_record of
        (Longident.t loc * label_description * pattern) list * closed_flag
    | Tpat_array of pattern list
    | Tpat_or of pattern * pattern * row_desc option
    | Tpat_lazy of pattern

  and expression = {
    exp_desc : expression_desc;
    exp_loc : Location.t;
    exp_extra : (exp_extra * Location.t * attribute list) list;
    exp_type : type_expr;
    exp_env : Env.t;
    exp_attributes : attribute list;
  }

  and exp_extra =
    | Texp_constraint of core_type
    | Texp_coerce of core_type option * core_type
    | Texp_open of override_flag * Path.t * Longident.t loc * Env.t
    | Texp_poly of core_type option
    | Texp_newtype of string

  and expression_desc =
    | Texp_ident of Path.t * Longident.t loc * Types.value_description
    | Texp_constant of constant
    | Texp_let of rec_flag * value_binding list * expression
    | Texp_function of {
        arg_label : arg_label;
        param : Ident.t;
        cases : case list;
        partial : partial;
      }
    | Texp_apply of expression * (arg_label * expression option) list
    | Texp_match of expression * case list * case list * partial
    | Texp_try of expression * case list
    | Texp_tuple of expression list
    | Texp_construct of
        Longident.t loc * constructor_description * expression list
    | Texp_variant of label * expression option
    | Texp_record of {
        fields : (Types.label_description * record_label_definition) array;
        representation : Types.record_representation;
        extended_expression : expression option;
      }
    | Texp_field of expression * Longident.t loc * label_description
    | Texp_setfield of
        expression * Longident.t loc * label_description * expression
    | Texp_array of expression list
    | Texp_ifthenelse of expression * expression * expression option
    | Texp_sequence of expression * expression
    | Texp_while of expression * expression
    | Texp_for of
        Ident.t
        * Parsetree.pattern
        * expression
        * expression
        * direction_flag
        * expression
    | Texp_send of expression * meth * expression option
    | Texp_new of unit
    | Texp_instvar of unit
    | Texp_setinstvar of unit
    | Texp_override of unit
    (* | Texp_letmodule of Ident.t * string loc * module_expr * expression *)
    | Texp_letexception of extension_constructor * expression
    | Texp_assert of expression
    | Texp_lazy of expression
    | Texp_object of unit
    (* | Texp_pack of module_expr *)
    | Texp_unreachable
    | Texp_extension_constructor of Longident.t loc * Path.t

  and meth = Tmeth_name of string

  and case = {
    c_lhs : pattern;
    c_guard : expression option;
    c_rhs : expression;
  }

  and record_label_definition =
    | Kept of Types.type_expr
    | Overridden of Longident.t loc * expression

  and value_binding = {
    vb_pat : pattern;
    vb_expr : expression;
    vb_attributes : attributes;
    vb_loc : Location.t;
  }

  and core_type = {
    (* mutable because of [Typeclass.declare_method] *)
    mutable ctyp_desc : core_type_desc;
    mutable ctyp_type : type_expr;
    ctyp_env : Env.t; (* BINANNOT ADDED *)
    ctyp_loc : Location.t;
    ctyp_attributes : attribute list;
  }

  and core_type_desc =
    | Ttyp_any
    | Ttyp_var of string
    | Ttyp_arrow of arg_label * core_type * core_type
    | Ttyp_tuple of core_type list
    | Ttyp_constr of Path.t * Longident.t loc * core_type list
    (* | Ttyp_object of object_field list * closed_flag *)
    | Ttyp_class of Path.t * Longident.t loc * core_type list
    | Ttyp_alias of core_type * string
    | Ttyp_variant of row_field list * closed_flag * label list option
    | Ttyp_poly of string list * core_type
    | Ttyp_package of package_type

  and package_type = {
    pack_path : Path.t;
    pack_fields : (Longident.t loc * core_type) list;
    (* pack_type : Types.module_type; *)
    pack_txt : Longident.t loc;
  }
end
