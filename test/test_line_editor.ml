open Bilk

let check_int msg expected actual =
  Alcotest.(check int) msg expected actual

(* --- cursor_row --- *)

let test_cursor_row_empty () =
  check_int "empty" 0 (Line_editor.cursor_row "" 0)

let test_cursor_row_single_line () =
  check_int "mid single" 0 (Line_editor.cursor_row "hello" 3);
  check_int "end single" 0 (Line_editor.cursor_row "hello" 5)

let test_cursor_row_multi_line () =
  let text = "abc\ndef\nghi" in
  check_int "start" 0 (Line_editor.cursor_row text 0);
  check_int "before newline" 0 (Line_editor.cursor_row text 3);
  check_int "after first newline" 1 (Line_editor.cursor_row text 4);
  check_int "second line" 1 (Line_editor.cursor_row text 6);
  check_int "after second newline" 2 (Line_editor.cursor_row text 8);
  check_int "end" 2 (Line_editor.cursor_row text 11)

(* --- cursor_col --- *)

let test_cursor_col_empty () =
  check_int "empty" 0 (Line_editor.cursor_col "" 0)

let test_cursor_col_single_line () =
  check_int "start" 0 (Line_editor.cursor_col "hello" 0);
  check_int "mid" 3 (Line_editor.cursor_col "hello" 3);
  check_int "end" 5 (Line_editor.cursor_col "hello" 5)

let test_cursor_col_multi_line () =
  let text = "abc\ndef\nghi" in
  check_int "start of first" 0 (Line_editor.cursor_col text 0);
  check_int "end of first" 3 (Line_editor.cursor_col text 3);
  check_int "start of second" 0 (Line_editor.cursor_col text 4);
  check_int "mid of second" 2 (Line_editor.cursor_col text 6);
  check_int "start of third" 0 (Line_editor.cursor_col text 8);
  check_int "end of third" 3 (Line_editor.cursor_col text 11)

(* --- num_lines --- *)

let test_num_lines () =
  check_int "empty" 1 (Line_editor.num_lines "");
  check_int "single" 1 (Line_editor.num_lines "hello");
  check_int "two" 2 (Line_editor.num_lines "a\nb");
  check_int "three" 3 (Line_editor.num_lines "a\nb\nc");
  check_int "trailing newline" 2 (Line_editor.num_lines "hello\n")

(* --- row_start --- *)

let test_row_start () =
  let text = "abc\ndef\nghi" in
  check_int "row 0" 0 (Line_editor.row_start text 0);
  check_int "row 1" 4 (Line_editor.row_start text 1);
  check_int "row 2" 8 (Line_editor.row_start text 2)

let test_row_start_varying_lengths () =
  let text = "a\nbc\ndef" in
  check_int "row 0" 0 (Line_editor.row_start text 0);
  check_int "row 1" 2 (Line_editor.row_start text 1);
  check_int "row 2" 5 (Line_editor.row_start text 2)

(* --- row_length --- *)

let test_row_length () =
  let text = "abc\ndef\nghi" in
  check_int "row 0" 3 (Line_editor.row_length text 0);
  check_int "row 1" 3 (Line_editor.row_length text 1);
  check_int "row 2" 3 (Line_editor.row_length text 2)

let test_row_length_varying () =
  let text = "a\nbcde\nfg" in
  check_int "row 0" 1 (Line_editor.row_length text 0);
  check_int "row 1" 4 (Line_editor.row_length text 1);
  check_int "row 2" 2 (Line_editor.row_length text 2)

let test_row_length_empty_line () =
  let text = "a\n\nb" in
  check_int "empty middle" 0 (Line_editor.row_length text 1)

(* --- pos_of_row_col --- *)

let test_pos_of_row_col () =
  let text = "abc\ndef\nghi" in
  check_int "(0,0)" 0 (Line_editor.pos_of_row_col text 0 0);
  check_int "(0,2)" 2 (Line_editor.pos_of_row_col text 0 2);
  check_int "(1,1)" 5 (Line_editor.pos_of_row_col text 1 1);
  check_int "(2,3)" 11 (Line_editor.pos_of_row_col text 2 3)

let test_pos_of_row_col_clamp () =
  let text = "a\nbcde\nf" in
  (* Row 0 has length 1, requesting col 5 → clamped to 1: pos = 0 + 1 = 1 *)
  check_int "clamp row 0" 1 (Line_editor.pos_of_row_col text 0 5);
  (* Row 2 starts at 7, has length 1, requesting col 3 → clamped: pos = 7 + 1 = 8 *)
  check_int "clamp row 2" 8 (Line_editor.pos_of_row_col text 2 3)

(* --- word_forward --- *)

let test_word_forward () =
  let text = "hello world" in
  check_int "from 0" 6 (Line_editor.word_forward text 0);
  check_int "from 6" 11 (Line_editor.word_forward text 6);
  check_int "at end" 11 (Line_editor.word_forward text 11)

let test_word_forward_scheme () =
  let text = "(define (f x) (+ x 1))" in
  (* From 0: '(' is non-word, skip word (none), skip non-word '(' → 1 *)
  check_int "past open paren" 1 (Line_editor.word_forward text 0);
  (* From 1: skip word 'define' → 7, skip non-word ' (' → 9 (at 'f') *)
  check_int "past define" 9 (Line_editor.word_forward text 1)

(* --- word_backward --- *)

let test_word_backward () =
  let text = "hello world" in
  check_int "from end" 6 (Line_editor.word_backward text 11);
  check_int "from 6" 0 (Line_editor.word_backward text 6);
  check_int "at start" 0 (Line_editor.word_backward text 0)

let test_word_backward_scheme () =
  let text = "(+ x 1)" in
  (* From 7 (end): skip non-word ')' at 6 → 5, skip word '1' at 5 → 4, result 5 *)
  check_int "from end to 1" 5 (Line_editor.word_backward text 7);
  (* From 5 (at '1'): skip non-word ' ' at 4 → 3, skip word 'x' at 3 → 2, result 3 *)
  check_int "from 5 to x" 3 (Line_editor.word_backward text 5)

(* --- Cross-line cursor movement via pos_of_row_col --- *)

let test_cursor_move_up () =
  let text = "abcdef\nghi" in
  (* Cursor at (1, 2) = pos 9, move up: (0, 2) = pos 2 *)
  let row = Line_editor.cursor_row text 9 in
  let col = Line_editor.cursor_col text 9 in
  check_int "row" 1 row;
  check_int "col" 2 col;
  let new_pos = Line_editor.pos_of_row_col text (row - 1) col in
  check_int "up" 2 new_pos

let test_cursor_move_down () =
  let text = "ab\ncdef" in
  (* Cursor at (0, 1) = pos 1, move down: (1, 1) = pos 4 *)
  let row = Line_editor.cursor_row text 1 in
  let col = Line_editor.cursor_col text 1 in
  check_int "row" 0 row;
  check_int "col" 1 col;
  let new_pos = Line_editor.pos_of_row_col text (row + 1) col in
  check_int "down" 4 new_pos

let test_cursor_move_up_clamp () =
  let text = "ab\ncdef" in
  (* Cursor at (1, 3) = pos 6, move up: (0, 3) → clamped to (0, 2) = pos 2 *)
  let row = Line_editor.cursor_row text 6 in
  let col = Line_editor.cursor_col text 6 in
  check_int "row" 1 row;
  check_int "col" 3 col;
  let new_pos = Line_editor.pos_of_row_col text (row - 1) col in
  check_int "up clamp" 2 new_pos

(* --- region_bounds --- *)

let check_region msg expected actual =
  let pp fmt = function
    | None -> Format.fprintf fmt "None"
    | Some (s, e) -> Format.fprintf fmt "Some (%d, %d)" s e
  in
  let eq a b = match a, b with
    | None, None -> true
    | Some (s1, e1), Some (s2, e2) -> s1 = s2 && e1 = e2
    | _ -> false
  in
  let testable = Alcotest.testable pp eq in
  Alcotest.check testable msg expected actual

let test_region_bounds_no_mark () =
  check_region "no mark" None (Line_editor.region_bounds None 5)

let test_region_bounds_mark_before_cursor () =
  check_region "mark before cursor" (Some (2, 7))
    (Line_editor.region_bounds (Some 2) 7)

let test_region_bounds_mark_after_cursor () =
  check_region "mark after cursor" (Some (3, 10))
    (Line_editor.region_bounds (Some 10) 3)

let test_region_bounds_mark_equals_cursor () =
  check_region "mark equals cursor" (Some (5, 5))
    (Line_editor.region_bounds (Some 5) 5)

(* --- apply_selection_overlay --- *)

let check_str msg expected actual =
  Alcotest.(check string) msg expected actual

let test_overlay_plain_text () =
  (* "hello world" with selection on visible columns [2, 5) → "he[7mllo[27m world" *)
  let result = Line_editor.apply_selection_overlay "hello world" 2 5 in
  check_str "plain mid" "he\x1b[7mllo\x1b[27m world" result

let test_overlay_full_line () =
  let result = Line_editor.apply_selection_overlay "abc" 0 3 in
  check_str "full line" "\x1b[7mabc\x1b[27m" result

let test_overlay_empty_selection () =
  let result = Line_editor.apply_selection_overlay "abc" 2 2 in
  check_str "empty selection" "abc" result

let test_overlay_with_ansi () =
  (* "\x1b[31mhello\x1b[0m world" — "hello" is red, selection on visible columns 2..7.
     The \x1b[0m reset inside the selection must be followed by \x1b[7m to
     re-establish reverse video, otherwise the space and 'w' lose highlighting. *)
  let input = "\x1b[31mhello\x1b[0m world" in
  let result = Line_editor.apply_selection_overlay input 2 7 in
  check_str "with ansi"
    "\x1b[31mhe\x1b[7mllo\x1b[0m\x1b[7m w\x1b[27morld" result

let test_overlay_multiple_resets () =
  (* Two styled tokens inside selection: each \x1b[0m must re-emit \x1b[7m.
     The opening \x1b[31m precedes any visible char, so \x1b[7m is injected
     at vis col 0 — after the color code but before the first char. *)
  let input = "\x1b[31ma\x1b[0m \x1b[32mb\x1b[0m c" in
  let result = Line_editor.apply_selection_overlay input 0 5 in
  check_str "multiple resets"
    ("\x1b[31m\x1b[7ma\x1b[0m\x1b[7m \x1b[32m\x1b[7mb\x1b[0m\x1b[7m c\x1b[27m")
    result

let test_overlay_at_start () =
  let result = Line_editor.apply_selection_overlay "hello" 0 3 in
  check_str "from start" "\x1b[7mhel\x1b[27mlo" result

let test_overlay_at_end () =
  let result = Line_editor.apply_selection_overlay "hello" 3 5 in
  check_str "to end" "hel\x1b[7mlo\x1b[27m" result

let () =
  Alcotest.run "Line_editor" [
    "cursor_row", [
      Alcotest.test_case "empty" `Quick test_cursor_row_empty;
      Alcotest.test_case "single line" `Quick test_cursor_row_single_line;
      Alcotest.test_case "multi line" `Quick test_cursor_row_multi_line;
    ];
    "cursor_col", [
      Alcotest.test_case "empty" `Quick test_cursor_col_empty;
      Alcotest.test_case "single line" `Quick test_cursor_col_single_line;
      Alcotest.test_case "multi line" `Quick test_cursor_col_multi_line;
    ];
    "num_lines", [
      Alcotest.test_case "basic" `Quick test_num_lines;
    ];
    "row_start", [
      Alcotest.test_case "equal lengths" `Quick test_row_start;
      Alcotest.test_case "varying lengths" `Quick test_row_start_varying_lengths;
    ];
    "row_length", [
      Alcotest.test_case "equal lengths" `Quick test_row_length;
      Alcotest.test_case "varying lengths" `Quick test_row_length_varying;
      Alcotest.test_case "empty line" `Quick test_row_length_empty_line;
    ];
    "pos_of_row_col", [
      Alcotest.test_case "basic" `Quick test_pos_of_row_col;
      Alcotest.test_case "clamp" `Quick test_pos_of_row_col_clamp;
    ];
    "word_forward", [
      Alcotest.test_case "basic" `Quick test_word_forward;
      Alcotest.test_case "scheme" `Quick test_word_forward_scheme;
    ];
    "word_backward", [
      Alcotest.test_case "basic" `Quick test_word_backward;
      Alcotest.test_case "scheme" `Quick test_word_backward_scheme;
    ];
    "cross-line cursor", [
      Alcotest.test_case "move up" `Quick test_cursor_move_up;
      Alcotest.test_case "move down" `Quick test_cursor_move_down;
      Alcotest.test_case "move up clamp" `Quick test_cursor_move_up_clamp;
    ];
    "region_bounds", [
      Alcotest.test_case "no mark" `Quick test_region_bounds_no_mark;
      Alcotest.test_case "mark before cursor" `Quick test_region_bounds_mark_before_cursor;
      Alcotest.test_case "mark after cursor" `Quick test_region_bounds_mark_after_cursor;
      Alcotest.test_case "mark equals cursor" `Quick test_region_bounds_mark_equals_cursor;
    ];
    "apply_selection_overlay", [
      Alcotest.test_case "plain mid" `Quick test_overlay_plain_text;
      Alcotest.test_case "full line" `Quick test_overlay_full_line;
      Alcotest.test_case "empty selection" `Quick test_overlay_empty_selection;
      Alcotest.test_case "with ansi" `Quick test_overlay_with_ansi;
      Alcotest.test_case "multiple resets" `Quick test_overlay_multiple_resets;
      Alcotest.test_case "from start" `Quick test_overlay_at_start;
      Alcotest.test_case "to end" `Quick test_overlay_at_end;
    ];
  ]
