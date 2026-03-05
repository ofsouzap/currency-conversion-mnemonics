open! Core

(** [calculate_all correct_rate] returns all mnemonics with at most 4 steps.
    Note that these are UNSORTED. Returns an error if [correct_rate] is not
    positive. *)
val calculate_all :
  correct_rate:Q.t -> unit -> (Mnemonic.t list, string) Result.t

(** [calculate correct_rate] returns the best mnemonic (the one with the lowest
    final score). Returns an error if [correct_rate] is not positive or if no
    solution is found. *)
val calculate : correct_rate:Q.t -> unit -> (Mnemonic.t, string) Result.t
