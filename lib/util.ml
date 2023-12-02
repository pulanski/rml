let format_time duration =
  match duration with
  | t when t >= 60. -> Printf.sprintf "%.2f m" (t /. 60.)
  | t when t >= 1. -> Printf.sprintf "%.2f s" t
  | t when t >= 0.001 -> Printf.sprintf "%.2f ms" (t *. 1000.)
  | t when t >= 0.000001 -> Printf.sprintf "%.2f μs" (t *. 1_000_000.)
  | t -> Printf.sprintf "%.2f nanoseconds" (t *. 1_000_000_000.)