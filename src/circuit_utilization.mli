(** Utilization information for a circuit which can be printed to a sexp.

    It tries to balance summarizing different node types to provide an overview of both
    usage and the potential implication on the critical path without being overly
    detailed.

    [and], [or], [xor] and [not] gates just print the total number of bits.

    [adder], [subtractors], [multipliers] and [comparators] show the total number of bits
    and the largest single node.

    [multiplexers] are grouped by depth and show the total number of bits and largest
    data width in each group.

    [memories] are grouped by their aspect ratio - data width and depth.

    [instantiations] are recursively processed if present in an optional
    [Circuit_database.t]. *)

open! Base

module Total_bits : sig
  type t =
    { count : int
    ; total_bits : int
    }
  [@@deriving sexp_of]
end

module Total_and_max_bits : sig
  type t =
    { count : int
    ; total_bits : int
    ; max_instance_bits : int
    }
  [@@deriving sexp_of]
end

module Multiplexer : sig
  type t =
    { max_instance_bits : int
    ; total_bits : int
    ; count : int
    }
  [@@deriving sexp_of]
end

module Multiplexers : sig
  module Mux_map : sig
    type t = Multiplexer.t Map.M(Int).t
  end

  type t =
    { count : int
    ; total_bits : int
    ; multiplexers : Mux_map.t
    }
  [@@deriving sexp_of]
end

module Memory : sig
  type t =
    { data_width : int
    ; depth : int
    ; total_bits : int
    }
  [@@deriving sexp_of, compare]

  include Comparator.S with type t := t
end

module Memories : sig
  module Mem_map : sig
    type t = int Map.M(Memory).t
  end

  type t =
    { count : int
    ; total_bits : int
    ; memories : Mem_map.t
    }
  [@@deriving sexp_of]
end

module Instantiation : sig
  type 'a t =
    | Instantiation of string
    | Submodule of 'a
  [@@deriving sexp_of]
end

module Instantiations : sig
  type 'a t = 'a Instantiation.t list [@@deriving sexp_of]
end

type t =
  { name : string
  ; adders : Total_and_max_bits.t option
  ; subtractors : Total_and_max_bits.t option
  ; unsigned_multipliers : Total_and_max_bits.t option
  ; signed_multipliers : Total_and_max_bits.t option
  ; and_gates : Total_bits.t option
  ; or_gates : Total_bits.t option
  ; xor_gates : Total_bits.t option
  ; not_gates : Total_bits.t option
  ; equals : Total_and_max_bits.t option
  ; comparators : Total_and_max_bits.t option
  ; multiplexers : Multiplexers.t option
  ; registers : Total_bits.t option
  ; memories : Memories.t option
  ; constants : Total_bits.t option
  ; wires : Total_bits.t option
  ; concatenation : Total_bits.t option
  ; part_selects : Total_bits.t option
  ; instantiations : t Instantiations.t option
  }
[@@deriving sexp_of]

(** Calculate the utilization of gates, rams, registers. If [database] is provided
    instantiations are recursively calculated as well. *)
val create : ?database:Circuit_database.t -> Circuit.t -> t
