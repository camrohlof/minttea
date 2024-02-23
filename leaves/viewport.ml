type border = {
  top : string;
  bottom : string;
  left : string;
  right : string;
  top_left : string;
  top_right : string;
  bottom_left : string;
  bottom_right : string;
}

type t = {
  start_of_frame : int;
  width : int;
  height : int;
  cursor : int;
  lines : string list;
  borders : border;
}

let default_borders =
  {
    top = "─";
    bottom = "─";
    left = "│";
    right = "│";
    top_left = "┌";
    top_right = "┐";
    bottom_left = "└";
    bottom_right = "┘";
  }

let hidden_borders =
  {
    top = " ";
    bottom = " ";
    left = " ";
    right = " ";
    top_left = " ";
    top_right = " ";
    bottom_left = " ";
    bottom_right = " ";
  }

let render_top_edge borders width =
  let top_line =
    Array.to_list (Array.make width borders.top) |> String.concat ""
  in
  Printf.sprintf "%s%s%s" borders.top_left top_line borders.top_right

let render_sides borders lines =
  let render_line content =
    Printf.sprintf "%s%s%s" borders.left content borders.right
  in
  lines |> List.map render_line

let render_bottom_edge borders width =
  let bottom_line =
    Array.to_list (Array.make width borders.bottom) |> String.concat ""
  in
  Printf.sprintf "%s%s%s" borders.bottom_left bottom_line borders.bottom_right

let apply_border t lines =
  let top = render_top_edge t.borders (t.width + 2) in
  let middle = render_sides t.borders lines in
  let bottom = render_bottom_edge t.borders (t.width + 2) in
  [ top ] @ middle @ [ bottom ]

let update t (e : Minttea.Event.t) =
  let view_up t steps =
    let cursor = max (t.cursor - steps) 0 in
    let start_of_frame =
      if cursor < t.start_of_frame then cursor else t.start_of_frame
    in
    { t with cursor; start_of_frame }
  in
  let view_down t steps =
    let cursor = min (t.cursor + steps) (List.length t.lines - 1) in
    let start_of_frame =
      if cursor > t.start_of_frame + t.height - 1 then cursor - t.height + 1
      else t.start_of_frame
    in
    { t with cursor; start_of_frame }
  in
  match e with
  | Minttea.Event.KeyDown (Key "b") -> view_up t t.height
  | KeyDown (Key "f") | KeyDown Space -> view_down t t.height
  | KeyDown (Key "k") | KeyDown Up -> view_up t 1
  | KeyDown (Key "j") | KeyDown Down -> view_down t 1
  | _ -> t

let truncate_or_pad_unicode ~width line =
  let uuseg_string_length s =
    Uuseg_string.fold_utf_8 `Grapheme_cluster (fun len _ -> len + 1) 0 s
  in
  let uuseg_string_sub s start n =
    let inner s start n acc =
      Uuseg_string.fold_utf_8 `Grapheme_cluster
        (fun (pos, collected, s) c ->
          if pos >= start && collected < n then (pos + 1, collected + 1, s ^ c)
          else (pos + 1, collected, s))
        acc s
    in
    let _, _, r = inner s start n (0, 0, "") in
    r
  in
  let len = uuseg_string_length line in
  if len > width then uuseg_string_sub line 0 (width - 1) ^ "..."
  else line ^ String.make (width - len) ' '

let view t =
  let visible_lines =
    t.lines
    |> List.map (fun line -> truncate_or_pad_unicode ~width:t.width line)
    |> List.to_seq |> Seq.drop t.start_of_frame |> Seq.take t.height
    |> List.of_seq
  in
  let formatted_lines = apply_border t visible_lines in
  Printf.sprintf "%s" (String.concat "\n" formatted_lines)
