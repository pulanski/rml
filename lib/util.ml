let format_time duration =
  match duration with
  | t when t >= 60. -> Printf.sprintf "%.2f m" (t /. 60.)
  | t when t >= 1. -> Printf.sprintf "%.2f s" t
  | t when t >= 0.001 -> Printf.sprintf "%.2f ms" (t *. 1000.)
  | t when t >= 0.000001 -> Printf.sprintf "%.2f μs" (t *. 1_000_000.)
  | t -> Printf.sprintf "%.2f ns" (t *. 1_000_000_000.)

type timing = {
  phase: string;
  duration: float;
}

let timings = ref []

let add_timing phase duration =
  timings := { phase; duration } :: !timings

let time_phase phase_name f =
  let start_time = Unix.gettimeofday () in
  let result = f () in
  let end_time = Unix.gettimeofday () in
  let duration = end_time -. start_time in
  add_timing phase_name duration;
  Printf.printf "%s in %s\n" phase_name (format_time duration);
  result


let report_timings () =
  let total_time = List.fold_left (fun acc timing -> acc +. timing.duration) 0.0 !timings in
  let comptime = format_time total_time in
  Printf.printf "\nTotal Compilation Time: %s\n" comptime;

  List.iter (fun timing ->
    let percent = (timing.duration /. total_time) *. 100. in
    Printf.printf "%s: %s (%.2f%%)\n" timing.phase (format_time timing.duration) percent
  ) !timings;

  Printf.printf "\nTime Distribution Chart:\n";
  let max_bar_width = 10 in
  List.iter (fun timing ->
    let proportion = (timing.duration /. total_time) in
    let bar_width = int_of_float (proportion *. float_of_int max_bar_width) in
    let bar = String.make bar_width '#' in
    Printf.printf "%12s | %s\n" timing.phase bar
  ) !timings


