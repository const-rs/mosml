(*
 * Types for mlb file.
 *
 * In contrast to MLton parsing, each .mlb file is scanned and
 * parsed separately. The included files are loaded only after AST
 * is fully created.
 *)

(* Errors appearing while loading and parsing .mlb files. *)
(* Failure to read, parse file or cycle in included .mlb *)
datatype fileError = ReadFailure | ParseFailure | CyclicDependency

datatype funBind = FunId of string | FunBind of string*string

datatype strBind = StrId of string | StrBind of string*string

datatype sigBind = SigId of string | SigBind of string*string

type basId = string

type annotation = string

datatype basDec = Basis of basBind list | Local of (basDec list)*(basDec list)
    | Open of basId list | Structure of strBind list | Signature of sigBind list 
    | Functor of funBind list | Path of includedFileType*string 
    | Annotation of (string list)*(basDec list)
    and basBind = BasBind of (basId)*(basExp)
    and basExp = Bas of basDec list | BasId of basId | Let of (basDec list)*basExp
    (* The type of referenced file - .mlb, .sig, .sml, .fun. For some
     * .mlb files from MLton distribution we also need Unknown. The type
     * of the file is determined by lexer by extension of the file.
     * MLBFile - not yet loaded .mlb, LoadedMLBFile - parse tree of successfully
     * loaded file, FailedMLBFile - why failed loading of the file.
     *)
    and includedFileType = UnknownFile | MLBFile | LoadedMLBFile of basDec list
      | FailedMLBFile of fileError | SIGFile | SMLFile | FUNFile

(* Currently (6 sept 2016) MLton, SMLNJ, MLkit use only SML_LIB and
 * TARGET_ARCH (mlton). Valid values for TARGET_ARCH are:
 * netbsd, solaris, hurd, darwin, freebsd, mingw, cygwin, linux,
 * openbsd, hpux, aix. For MosML TARGET_ARCH=linux.
 *)
val pathVariables = ref
    [("SML_LIB","/sml-lib-location"),
     ("HOME","/home"), (* for automated testing, remove in future *)
     ("TARGET_ARCH", "bytecode"),
     ("TARGET_OS", "linux"),
     ("DEFAULT_INT", "int32"),
     ("DEFAULT_WORD", "word32"),
     ("DEFAULT_REAL", "real64")]

(* Sets path variable to value. If path variable is not
 * yet defined, adds it. *)
fun setPathVariable variable value =
    case List.find (fn (a,_) => a = variable) (!pathVariables) of
      SOME _ => pathVariables :=
            map 
                (fn (variable', value') => 
                    if variable' <> variable then
                        (variable', value')
                    else
                        (variable, value)
                ) (!pathVariables)
    | NONE => pathVariables := (variable, value)::(!pathVariables)

(* Returns the value of path variable by its name.
 * Currently works only with hardcoded predefined 
 * path variables. *)
fun pathVariable variable =
    let
        val sub = List.find 
            (fn (name, _) => (String.compare (name, variable)) = EQUAL)
            (!pathVariables)
    in
        case sub of
          SOME (name, value) => value
        | NONE => raise Fail ("Unknown path variable " ^ variable)
    end

