open Owi
open Syntax

let load_func_from_module ls mod_id f_name =
  let* exports, env_id =
    match mod_id with
    | None -> begin
      match ls.Link.last with
      | None -> Error `Unbound_last_module
      | Some m -> Ok m
    end
    | Some mod_id -> (
      match Link.StringMap.find_opt mod_id ls.Link.by_id with
      | None -> Error (`Unbound_module mod_id)
      | Some exports -> Ok exports )
  in
  match Link.StringMap.find_opt f_name exports.functions with
  | None -> Error (`Unbound_name f_name)
  | Some v -> Ok (v, env_id)

let run_modul () =

  let* m = Owi.Parse.Binary.Module.from_file (Fpath.v "./patched.wasm") in

  let link_state =
    Link.extern_module Link.empty_state ~name:"env"
      Concrete_wasm_ffi.env_extern_module
  in
  let link_state =
    Link.extern_module link_state ~name:"wasi_snapshot_preview1"
      Concrete_wasm_ffi.wasi_snapshot_preview1_extern_module
  in

  let* m, link_state = Link.modul link_state ~name:None m in

  let* () = Interpret.Concrete.modul link_state.envs m in

  let* f, env_id = load_func_from_module link_state None "sqlite3_open" in

  Interpret.Concrete.exec_vfunc_from_outside ~locals:[ V.I32 0l; I32 0l ] ~env:env_id
    ~envs:link_state.envs f

let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  let result = run_modul () in
  match result with
  | Error e ->
    let s = Owi.Result.err_to_string e in
    Fmt.pr "error: %s@\n" s;
    assert false
  | Ok _ -> ()
