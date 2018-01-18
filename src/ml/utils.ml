open Lexing
open Constants

let msg x =
  let open Format in
  (if !debug then fprintf err_formatter x else ifprintf err_formatter x)

let pr_pos ppf pos =
  Format.fprintf ppf "%s:%d:%d"
                 pos.pos_fname
                 pos.pos_lnum
                 (pos.pos_cnum - pos.pos_bol + 1)
