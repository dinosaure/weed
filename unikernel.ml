let () = Miou_solo5.(run []) @@ fun () ->
  Logs.set_level (Some Logs.Info);
    Logs.set_reporter (Logs_fmt.reporter ());
  let m = {| (module (func $f i32.const 42 drop) (start $f))|} in
  let m = Owi.Parse.Text.Module.from_string m in
  match m with
  | Error e ->
      let s = Owi.Result.err_to_string e in
      Fmt.pr "error: %s@\n" s;
      assert false
  | Ok m ->
      let result =
      Owi.Compile.Text.until_interpret ~unsafe:false ~rac:false ~srac:false ~optimize:false ~name:None Owi.Link.empty_state m
      in
      match result with
      | Error e ->

      let s = Owi.Result.err_to_string e in
      Fmt.pr "error: %s@\n" s;
      assert false
      | Ok _ -> ()
