open Core

module type S = sig
  type 'a content

  type content_key

  type 'a t

  val make : unit -> 'a t

  val new_level : 'a t -> 'a t

  val add : 'a t -> content_key -> 'a -> unit

  val find : 'a t -> content_key -> ('a * 'a t) option

  val find_local : 'a t -> content_key -> ('a * 'a t) Core.Option.t

  val update : 'a t -> content_key -> 'a -> 'a t Core.Option.t

  val level_values : 'a t -> 'a list
end

module StrMap : Map.S_plain with type Key.t = string

module ListScope : S with type 'a content = 'a list and type content_key = unit

module StrMapScope :
  S with type 'a content = 'a StrMap.t and type content_key = StrMap.Key.t

module OneElemScope :
  S with type 'a content = 'a option and type content_key = unit
