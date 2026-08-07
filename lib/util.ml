let flip f = fun x y -> f y x

let char_ptr_buffer_of_string =
  Ctypes.(coerce string (ptr char))

let char_ptr_opt_buffer_of_string = function
  | None -> None
  | Some s -> Some (char_ptr_buffer_of_string s)

module Option = struct
  let map f = function
    | Some x -> Some (f x)
    | None -> None

  let some = function
    | Some x -> x
    | None -> failwith "Option.some: None"
end
