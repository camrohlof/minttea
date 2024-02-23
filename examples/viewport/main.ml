open Minttea
open Leaves

type model = { viewport : Viewport.t }

let initial_model =
  {
    viewport =
      {
        lines =
          [
            "1 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "2 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "3 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "4 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "5 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "6 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "7 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "8 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "9 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "10 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "11 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "12 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "13 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "14 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "15 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "16 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "17 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
            "18 Lorem ipsum dolor sit amet, qui minim labore adipisicing minim \
             sint cillum sint consectetur cupidatat.";
          ];
        cursor = 0;
        start_of_frame = 0;
        height = 10;
        width = 100;
        borders = Viewport.default_borders;
      };
  }

let init _ = Command.Noop

let update event model =
  match event with
  | Event.KeyDown (Key "q") -> (model, Command.Quit)
  | Event.KeyDown _ ->
      ({ viewport = Viewport.update model.viewport event }, Command.Noop)
  | _ -> (model, Command.Noop)

let view model =
  Viewport.view model.viewport ^ "\n\nhint: up/down b/f/space, quit: q"

let app = Minttea.app ~init ~update ~view ()
let () = Minttea.start ~initial_model app
