#use "topfind";;
#require "tsdl.top";;

open Tsdl
open Result

(** Code to practice working with TSDL and OpenGL *)
(** Opens GL window *)

let main () = match Sdl.init Sdl.Init.video with
| Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
| Ok () ->
    match Sdl.create_window ~w:640 ~h:480 "SDL OpenGL" Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok w ->
        Sdl.delay 3000l;
        Sdl.destroy_window w;
        Sdl.quit ();
        exit 0

let () = main ()
