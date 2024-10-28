module S = Mariadb.Nonblocking.Status

module Wait = struct
  module IO = struct
    type 'a future = 'a

    let ( >>= ) x f = f x
    let return x = x
  end

  let return = IO.return

  let wait mariadb status =
    let fd = Mariadb.Nonblocking.fd mariadb in
    let rfd = if S.read status then [ fd ] else [] in
    let wfd = if S.write status then [ fd ] else [] in
    let efd = if S.except status then [ fd ] else [] in
    let timeout =
      if S.timeout status then float @@ Mariadb.Nonblocking.timeout mariadb
      else -1.0
    in
    try
      let rfd, wfd, efd = Unix.select rfd wfd efd timeout in
      return
      @@ S.create ~read:(rfd <> []) ~write:(wfd <> []) ~except:(efd <> []) ()
    with Unix.Unix_error (_, _, _) -> return @@ S.create ~timeout:true ()
end

let () =
  let open Printf in
  let open Wait.IO in
  let module M = Mariadb.Nonblocking.Make (Wait) in
  let ( >|= ) m f = m >>= fun x -> return (f x) in
  let env var def = try Sys.getenv var with Not_found -> def in

  let or_die where = function
    | Ok r -> return r
    | Error (i, e) ->
        eprintf "%s: (%d) %s\n%!" where i e;
        exit 2
  in
  let execute_no_data stmt =
    M.Stmt.execute stmt [||] >>= or_die "execute" >|= fun res ->
    assert (M.Res.num_rows res = 0)
  in
  let connect () =
    M.connect
      ~host:(env "OCAML_MARIADB_HOST" "127.0.0.1")
      ~user:(env "OCAML_MARIADB_USER" "root")
        (* ~pass:(env "OCAML_MARIADB_PASS" "") *)
      ~db:(env "OCAML_MARIADB_DB" "experiment")
      ~port:(env "OCAML_MARIADB_PORT" "4319" |> int_of_string)
      ()
  in
  let string_of_timestamp t =
    let y, mon, day = M.Time.(year t, month t, day t) in
    let h, m, s, us = M.Time.(hour t, minute t, second t, microsecond t) in
    sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%06d" y mon day h m s us
  in
  let string_of_value = function
    | `Null -> "NULL"
    | `Int i -> sprintf "(%d : int)" i
    | `Float x -> sprintf "(%.8g : float)" x
    | `String s -> sprintf "(%S : string)" s
    | `Bytes s -> sprintf "(%S : bytes)" (Bytes.to_string s)
    | `Time t -> string_of_timestamp t
  in
  let equal_float x x' =
    abs_float (x -. x') /. (abs_float (x +. x') +. epsilon_float) < 1e-6
  in
  let equal_time t t' =
    let open M.Time in
    let open Stdlib in
    (* Treat `Datetime and `Timestamp as equal. *)
    year t = year t'
    && month t = month t'
    && day t = day t'
    && hour t = hour t'
    && minute t = minute t'
    && second t = second t'
  in
  let equal_field v v' =
    match (v, v') with
    | `Null, `Null -> true
    | `Null, _ | _, `Null -> false
    | `Int i, `Int i' -> i = i'
    | `Int i, `Float x | `Float x, `Int i -> float_of_int i = x
    | `Int _, _ | _, `Int _ -> false
    | `Float x, `Float x' -> equal_float x x'
    | `Float _, _ | _, `Float _ -> false
    | `String s, `String s' -> s = s'
    | `String s, `Bytes s' | `Bytes s', `String s -> s = Bytes.to_string s'
    | `String _, _ | _, `String _ -> false
    | `Bytes s, `Bytes s' -> s = s'
    | `Bytes _, _ | _, `Bytes _ -> false
    | `Time t, `Time t' -> equal_time t t'
  in
  let assert_field_equal v v' =
    if not (equal_field v v') then (
      eprintf "Parameter %s came back as %s.\n%!" (string_of_value v)
        (string_of_value v');
      exit 2)
  in
  let rec iter_s_list f = function
    | [] -> return ()
    | x :: xs -> f x >>= fun () -> iter_s_list f xs
  in
  let _test_integer () =
    connect () >>= or_die "connect" >>= fun dbh ->
    M.prepare dbh
      "CREATE TABLE IF NOT EXISTS ocaml_mariadb_test (id integer PRIMARY KEY \
       AUTO_INCREMENT, value integer, value_unsigned integer unsigned)"
    >>= or_die "prepare create"
    >>= fun create_table_stmt ->
    execute_no_data create_table_stmt >>= fun () ->
    let check (value : [ `Signed of int | `Unsigned of int ]) =
      let column =
        match value with
        | `Signed _ -> "value"
        | `Unsigned _ -> "value_unsigned"
      in
      M.prepare dbh
        (Printf.sprintf "INSERT INTO ocaml_mariadb_test (%s) VALUES (?)" column)
      >>= or_die "prepare insert"
      >>= fun insert_stmt ->
      let value_to_insert =
        match value with `Signed n -> n | `Unsigned n -> n
      in
      M.Stmt.execute insert_stmt [| `Int value_to_insert |] >>= or_die "insert"
      >>= fun res ->
      M.prepare dbh
        (Printf.sprintf "SELECT %s FROM ocaml_mariadb_test WHERE id = (?)"
           column)
      >>= or_die "prepare select"
      >>= fun select_stmt ->
      M.Stmt.execute select_stmt [| `Int (M.Res.insert_id res) |]
      >>= or_die "Stmt.execute"
      >>= M.Res.fetch (module M.Row.Array)
      >>= or_die "Res.fetch"
      >>= function
      | Some [| inserted_value |] ->
          assert_field_equal
            (`Int (M.Field.int inserted_value))
            (`Int value_to_insert)
      | _ -> assert false
    in
    let input =
      [
        `Signed (Int32.max_int |> Int32.to_int);
        `Signed (Int32.min_int |> Int32.to_int);
        `Unsigned (Int32.max_int |> Int32.to_int);
      ]
    in
    iter_s_list check input >>= fun () -> M.close dbh
  in
  let _test_integer, test_bigint =
    let make_check type_ =
      connect () >>= or_die "connect" >>= fun dbh ->
      M.prepare dbh
        (Printf.sprintf
           "CREATE TABLE IF NOT EXISTS ocaml_mariadb_test (id integer PRIMARY \
            KEY AUTO_INCREMENT, value %s, value_unsigned %s unsigned)"
           type_ type_)
      >>= or_die "prepare create"
      >>= fun create_table_stmt ->
      execute_no_data create_table_stmt >>= fun () ->
      let check (value : [ `Signed of int | `Unsigned of int ]) =
        let column =
          match value with
          | `Signed _ -> "value"
          | `Unsigned _ -> "value_unsigned"
        in
        M.prepare dbh
          (Printf.sprintf "INSERT INTO ocaml_mariadb_test (%s) VALUES (?)"
             column)
        >>= or_die "prepare insert"
        >>= fun insert_stmt ->
        let value_to_insert =
          match value with `Signed n -> n | `Unsigned n -> n
        in
        M.Stmt.execute insert_stmt [| `Int value_to_insert |]
        >>= or_die "insert"
        >>= fun res ->
        M.prepare dbh
          (Printf.sprintf "SELECT %s FROM ocaml_mariadb_test WHERE id = (?)"
             column)
        >>= or_die "prepare select"
        >>= fun select_stmt ->
        M.Stmt.execute select_stmt [| `Int (M.Res.insert_id res) |]
        >>= or_die "Stmt.execute"
        >>= M.Res.fetch (module M.Row.Array)
        >>= or_die "Res.fetch"
        >>= function
        | Some [| inserted_value |] ->
            assert_field_equal (`Int value_to_insert)
              (`Int (M.Field.int inserted_value))
        | _ -> assert false
      in
      (dbh, check)
    in
    let test_integer () =
      let dbh, check = make_check "integer" in
      let input =
        [
          `Signed
            (Int32.max_int |> Int32.to_int (* max value for integer column *));
          `Signed
            (Int32.min_int |> Int32.to_int (* min value for integer column *));
          `Unsigned (Unsigned.UInt32.max_int |> Unsigned.UInt32.to_int)
          (* max value for unsgined integer column.
             Produces the following error: insert: (1264) Out of range value for column 'value_unsigned' at row 1 *);
        ]
      in
      iter_s_list check input >>= fun () -> M.close dbh
    in
    let test_bigint () =
      let dbh, check = make_check "bigint" in
      let input =
        [
          `Signed Int.max_int
          (* [Int.max_int] is below the max value for bigint column (which is equivalent to [Int64.max_int])
             Produces the following error: Parameter (4611686018427387903 : int) came back as (-1 : int) *);
          `Unsigned Int.max_int
          (* insert: (1264) Out of range value for column 'value_unsigned' at row 1 *);
        ]
      in
      iter_s_list check input >>= fun () -> M.close dbh
    in
    (test_integer, test_bigint)
  in
  test_bigint ()
