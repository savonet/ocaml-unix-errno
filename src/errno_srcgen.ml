(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

let file = Sys.argv.(1)
let ic = open_in file
let buf = Buffer.create 1024

let () = try while true do
      let line = input_line ic in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n';
    done with End_of_file -> ()

let defns = Errno.defns_of_string (Buffer.contents buf)

let () = print_endline "(* Generated by errno-srcgen *)\n"

let () = print_endline "let defns = Errno.({"

open Printf

let field_of_errno errno = String.lowercase (Errno.to_string errno)

let () = Errno.iter_defns defns
    (fun i errno -> printf "  %s = Some %d;\n" (field_of_errno errno) i)
    (fun errno   -> printf "  %s = None;\n" (field_of_errno errno))

let () = print_endline "})\n"

let () = print_endline "let host = Errno.Host.of_defns defns"
