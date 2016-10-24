type 'a t
(** The collection type with

    * each element is associated with its distance
    * a soft limit of the number of elements in the collection
    * adding an element to the collection may cause the collection forget the elements with the biggest distances, so that the number of the elements in it would stay around the soft limit
 *)

val create : thresh:int -> limit:int -> 'a t
(** Create the collection with the given initial distance threshold 
    and the soft limit of the number of elements *)

val add : 'a t -> int -> 'a -> 'a t
(** Add an element with a distance. This may cause the collection forget the elements with the currently biggest distances so that the number of the elements in it would stay around the soft limit *)

val to_list : 'a t -> (int * 'a list) list
(** Get the stored elements with their distances *)

val thresh : 'a t -> int
(** Get the current distance threshold *)
