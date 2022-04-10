(*
                          CS 51 Problem Set
                 Simulation of an Infectious Disease

                      People in the simulation
 *)

module G = Graphics ;;
open Config ;;
open Registry ;;
module Ctr = Counter ;;
module Viz = Visualization ;;
module Stat = Statistics ;; 
(* module Utilities = Utilities ;; *) 

(*....................................................................
                                People
 *)
  
class person (initx : int) (inity : int)
             (initstepsize : int)
             (initinfect : float) =
  object (self)
    val id : string = Utilities.gensym ()
    val mutable posx : int = initx
    val mutable posy : int = inity
    val mutable step_size : int = initstepsize
    val mutable infectiousness : float = initinfect
    val mutable grocery : bool = false
    val mutable old_posx : int = 0
    val mutable old_posy : int = 0
    val mutable quar : bool = false

    method get_q : bool = quar
    method set_q : unit = quar <- not quar
  
    method id : string = id
    method step_size : int = step_size
    method infectiousness : float = infectiousness
                  
    method set_pos (x : int) (y : int) : unit =
      posx <- x;
      posy <- y
    method pos = posx, posy
                         
    method set_step_size (new_step_size : int) : unit =
      step_size <- new_step_size
                     
    method set_infectiousness (new_infect : float) : unit =
      infectiousness <- new_infect

    method move : unit =
      let x, y = self#pos in
      let newx, newy =
        Utilities.rand_step x y self#step_size in
      (* drop from old location in registry *)
      Reg.deregister (self :> thing_type);
      (* update location *)
      self#set_pos newx newy;
      (* re-add at the new location *)
      Reg.register (self :> thing_type)

    method fix_position : unit = 
      old_posx <- posx;
      old_posy <- posy

    method go_fixed : unit = 
      self#move_point old_posx old_posy

    method move_point (x : int) (y : int) : unit = 
        Reg.deregister (self :> thing_type);
        self#set_pos x y;
        Reg.register (self :> thing_type)

    method update : unit =
      self#move;
      (* Move the person to the grocery *)
      if cMODE = Grocery then
        begin
          if grocery 
          then 
            begin
              self#go_fixed;
              grocery <- false
            end
          else
            begin
              if Utilities.flip_coin cGROCERY 
              then
                begin
                grocery <- true;
                self#fix_position;
                self#move_point (cX_DIMENSION / 2) (cY_DIMENSION / 2)
                end
            end
        end
      else
      (* Random travelling *)
          begin
            if cMODE = Travel then
              if Utilities.flip_coin cTRAVEL then
                let newx, newy = 
                  Utilities.rand_step (cX_DIMENSION / 2) 
                                      (cY_DIMENSION / 2) 
                                      (cX_DIMENSION / 2) in
                self#move_point newx newy
          end
      
  
    method draw : unit =
      let x, y = self#pos in
      Viz.draw_circle x y G.black
  end ;;

(*....................................................................
                       People in various states

  Note that since these classes refer to each other, they must be
  simultaneously defined using `and` instead of sequentially defined
  as separate classes.
 *)
  
class susceptible (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_SUSCEPTIBLE
                   cINFECTIOUSNESS_SUSCEPTIBLE
            as super

    initializer
      Stat.susceptible#bump
                     
    method! update =
      super#update;
      let posx, posy = self#pos in
      (* calculate total infectiousness of all neighbors *)
      let infectiousness_total =
        Utilities.sum_float
	  (List.map (fun obj -> obj#infectiousness)
                    (Reg.neighbors (self :> thing_type))) in
      (* if infected, update the registry by replacing this object
         with an infected one *)
      if Utilities.flip_coin infectiousness_total then
        begin
          Stat.susceptible#debump;
          Reg.deregister (self :> thing_type);
          Reg.register ((new infected posx posy) :> thing_type)
        end

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_SUSCEPTIBLE
  end

and (* class *) infected (initx : int) (inity : int) =
  object (this)
    inherit person initx inity
                   cSTEP_SIZE_INFECTED
                   cINFECTIOUSNESS_INFECTED
            as super

    initializer
      Stat.infected#bump

    val mutable steps_left = 
      let mean, stdev = cRECOVERY_PERIOD in 
      Utilities.gaussian mean stdev |>
      int_of_float

    (*.................................................................
      Place any augmentations to `infected` here.
    ................................................................ *)

    method! update : unit = 
      super#update;
      (* Move the person to the quarantine *)
      if cMODE = Quarantine && not super#get_q then
        if Utilities.flip_coin Config.cQUARANTINE
          then
            begin
                super#set_q;
                super#fix_position;
                super#move_point (cX_DIMENSION-10) (cY_DIMENSION-10)
            end;
      let posx, posy = this#pos in
        if steps_left <= 0
        then
          begin
            Stat.infected#debump;
            let dead = Utilities.flip_coin cMORTALITY in
            if dead
            then Reg.register ((new deceased posx posy) :> thing_type)
            else 
              (* Move the person home if they were on quarantine *)
              begin
                if cMODE = Quarantine && super#get_q then super#go_fixed;
                Reg.register ((new recovered posx posy) :> thing_type)
              end;
            Reg.deregister (this :> thing_type)
          end
        else
          begin
            steps_left <- steps_left - 1
          end

    method! draw : unit = 
      let x, y = this#pos in
      Viz.draw_circle x y cCOLOR_INFECTED;
      Viz.draw_circle ~size:cNEIGHBOR_RADIUS ~filled:false x y cCOLOR_INFECTED
  end

and (* class *) recovered (initx : int) (inity : int) = 
  object (this)
    inherit person initx inity
                   cSTEP_SIZE_RECOVERED
                   cINFECTIOUSNESS_RECOVERED
            as super

    initializer
      Stat.recovered#bump

    val mutable steps_left = 
      let mean, stdev = cIMMUNITY_PERIOD in 
      Utilities.gaussian mean stdev |>
      int_of_float

    method! update : unit = 
      super#update;
      let posx, posy = this#pos in
      if steps_left <= 0
      then
        begin
          Stat.recovered#debump;
          Reg.deregister (this :> thing_type);
          Reg.register ((new susceptible posx posy) :> thing_type)
        end
      else
        steps_left <- steps_left - 1

    method! draw : unit =
      let x, y = this#pos in
      Viz.draw_circle x y cCOLOR_RECOVERED
  end

and (* class *) deceased (initx : int) (inity : int) = 
  object (this)
    inherit person initx inity
                  cSTEP_SIZE_DECEASED
                  cINFECTIOUSNESS_DECEASED
            as super

    initializer
      Stat.deceased#bump

    method! update : unit = 
      ()

    method! draw : unit =
      let x, y = this#pos in
      Viz.draw_cross x y cCOLOR_DECEASED
  end

(*....................................................................
Place definitions for any other classes here. In particular, you'll
want to at least implement a `recovered` class for `infected` people
who have recovered from the infection.
....................................................................*)
;;
