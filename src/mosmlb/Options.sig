(** Options and their loading code. NOTE: options, that affect logging
 *  are declared in Log stucture (failEarly and debug). *)
signature Options = 
sig
    (* Global options of the program *)

    val alwaysMake : bool ref
    val imitate : bool ref
    val execFile : string option ref
    val mlbFile : string option ref

    val init : unit -> unit
end
