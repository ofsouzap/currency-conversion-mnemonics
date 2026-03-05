open! Core

module Graph = struct
  module Cost = Int
  module State = Q_with_sexp

  module Transition = struct
    type t = { from : State.t; step : Mnemonic.Step.t }

    let to_ { from; step } = Mnemonic.Step.apply step from
    let cost { step; _ } = Mnemonic.Step.score step
    let to_step { step; _ } = step
  end

  let initial_state = Q.one

  let transitions q =
    List.map Mnemonic.Step.all ~f:(fun step -> Transition.{ from = q; step })
end

module Dijkstra = Dijkstra.Make (Graph)

module Scorable_mnemonic = struct
  include Mnemonic

  let score t =
    let complexity = List.sum (module Int) ~f:Step.score (steps t) in
    let error = error t in
    Q.(of_int complexity + (Q.of_int 50 * error))

  let compare x1 x2 = Q.compare (score x1) (score x2)
end

let calculate_all ~correct_rate () =
  if Q.(correct_rate <= zero) then Error "Correct rate must be positive"
  else
    Ok
      (let shortest_paths =
         Dijkstra.compute_shortest_paths ~maximum_path_length_incl:4 ()
       in

       let mnemonics =
         List.map (Dijkstra.Shortest_paths.to_list shortest_paths)
           ~f:(fun (_, path) ->
             let steps = List.map path ~f:Graph.Transition.to_step in
             Mnemonic.make ~correct_rate ~steps)
       in

       mnemonics)

let calculate ~correct_rate () =
  let open Result.Let_syntax in
  let%bind all_mnemonics = calculate_all ~correct_rate () in
  let best_mnemonic =
    List.min_elt all_mnemonics ~compare:Scorable_mnemonic.compare
  in
  match best_mnemonic with
  | None -> Error "No solution found"
  | Some mnemonic -> Ok mnemonic
