open Base
open Async_unix

module T = struct
  type t = Async_unix.Log.t
  type return_type = unit

  let would_log = Log.would_log
  let sexp ?level ?tags t msg = Log.sexp ?level ?tags t msg
  let default = ()
end

include T

module Global = struct
  type return_type = unit

  let default = ()
  let would_log = Log.Global.would_log
  let sexp ?level ?tags msg = Log.Global.sexp ?level ?tags msg
end

module No_global = struct
  module Ppx_log_syntax = struct
    include T

    module Global = struct
      type return_type = [ `Do_not_use_because_it_will_not_log ]

      let default = `Do_not_use_because_it_will_not_log
      let would_log _ = false
      let sexp ?level:_ ?tags:_ _ = `Do_not_use_because_it_will_not_log
    end
  end
end
