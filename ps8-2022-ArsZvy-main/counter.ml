(*
                          CS 51 Problem Set
                 Simulation of an Infectious Disease

                     Increment/Decrement Counters
 *)

class type counter_type =
  object
    (* set n -- Sets the running count to `n`. *)
    method set : int -> unit
    (* reset -- Resets the running count to zero. *)
    method reset : unit
    (* bump -- Increments the count. *)
    method bump : unit
    (* debump -- Decrements the count. *)
    method debump : unit
    (* count -- Returns the current count, initially zero. *)
    method count : int
  end ;;

(*....................................................................
Place your implementation of the `counter` class of class type
`counter_type` here.
....................................................................*)

class counter : counter_type = 
  object (this)

    val mutable num = 0

    method set (n : int) : unit =
      num <- n

    method reset : unit = 
      num <- 0

    method bump : unit = 
      num <- num + 1

    method debump : unit = 
      num <- num - 1

    method count : int = 
      num
  end;;

   
(*======================================================================
Reflection on the problem set

After each problem set, we'll ask you to reflect on your experience.
We care about your responses and will use them to help guide us in
creating and improving future assignments.

........................................................................
Please give us an honest (if approximate) estimate of how long (in
minutes) this problem set (in total, not just this file) took you to
complete.
......................................................................*)

let minutes_spent_on_pset () : int =
  failwith "time estimate not provided" ;;

(*......................................................................
It's worth reflecting on the work you did on this problem set, where
you ran into problems and how you ended up resolving them. What might
you have done in retrospect that would have allowed you to generate as
good a submission in less time? Please provide us your thoughts in the
string below.
......................................................................*)

let reflection () : string =
  "...your reflections here..." ;;
