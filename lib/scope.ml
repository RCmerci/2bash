open Core

module type Content = sig
  type 'a t

  type key

  val empty : 'a t

  val add : 'a t -> key -> 'a -> 'a t

  val find : 'a t -> key -> 'a option

  val data : 'a t -> 'a list
end

module ListContent = struct
  type 'a t = 'a list

  type key = unit

  let empty = []

  let add r _ v = v :: r

  let find r _ = None

  let data r = r
end

module StringMapContent = struct
  module StrMap = Map.Make_plain (String)

  type 'a t = 'a StrMap.t

  type key = StrMap.Key.t

  let empty = StrMap.empty

  let add r key data = StrMap.add r ~key ~data

  let find r key = StrMap.find r key

  let data r = StrMap.data r
end

module Make (Content : Content) = struct
  type 'a t = {mutable map: 'a Content.t; upper: 'a t option}

  let make () = {map= Content.empty; upper= None}

  let new_level scope =
    let scope' = make () in
    {scope' with upper= Some scope}


  let add scope key data =
    let origin = scope.map in
    let new' = Content.add origin key data in
    scope.map <- new'


  let rec find scope key =
    let data = Content.find scope.map key in
    if Option.is_none data then
      if Option.is_none scope.upper then None
      else find (Option.value_exn scope.upper) key
    else Some (Option.value_exn data, scope)


  let update scope key data =
    let r = find scope key in
    let open Option in
    r
    >>= fun (_, scope') ->
    add scope' key data |> return >>= fun _ -> return scope'


  let level_values scope = Content.data scope.map
end

module ListScope = Make (ListContent)
module StrMapScope = Make (StringMapContent)
