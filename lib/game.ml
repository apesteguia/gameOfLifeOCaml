module Game = struct
  type cell =
    | Dead
    | Alive

  type world =
    { size : int
    ; alive : int
    ; generation : int
    ; cells : cell array array
    }

  let init_world size =
    { size; alive = 0; generation = 0; cells = Array.make_matrix size size Dead }
  ;;

  let alive_random w =
    let randoms =
      w.cells
      |> Array.map (fun line ->
        line |> Array.map (fun _ -> if Random.int 2 = 0 then Alive else Dead))
    in
    { w with cells = randoms }
  ;;

  let count_alive_neighbors w x y =
    let count = ref 0 in
    for i = x - 1 to x + 1 do
      for j = y - 1 to y + 1 do
        if (i <> x || j <> y) && i >= 0 && j >= 0 && i < w.size && j < w.size
        then if w.cells.(i).(j) = Alive then count := !count + 1
      done
    done;
    !count
  ;;

  let update_cell w x y =
    let alive_neighbors = count_alive_neighbors w x y in
    match w.cells.(x).(y) with
    | Alive when alive_neighbors < 2 -> Dead
    | Alive when alive_neighbors > 3 -> Dead
    | Dead when alive_neighbors = 3 -> Alive
    | cell -> cell
  ;;

  let update w =
    let new_cells = Array.make_matrix w.size w.size Dead in
    let alive_count_ref = ref 0 in
    for i = 0 to w.size - 1 do
      for j = 0 to w.size - 1 do
        new_cells.(i).(j) <- update_cell w i j;
        if new_cells.(i).(j) = Alive then alive_count_ref := !alive_count_ref + 1
      done
    done;
    { w with cells = new_cells; generation = w.generation + 1; alive = !alive_count_ref }
  ;;
end
