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

let load_memory_from_module ls mod_id name =
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
  match Link.StringMap.find_opt name exports.memories with
  | None -> Error (`Unbound_name name)
  | Some v -> Ok (v, env_id)

let block_device_to_string size blk =
  let pagesize = Miou_solo5.Block.pagesize blk in
  let size_aligned = (size + pagesize - 1) / pagesize * pagesize in
  let bstr = Bstr.create size_aligned in
  for i = 0 to (size_aligned / pagesize) - 1 do
    Miou_solo5.Block.atomic_read blk ~off:(i * pagesize) ~dst_off:(i * pagesize) bstr
  done;
  Bstr.sub_string bstr ~off:0 ~len:size

let run_modul ~size blk =
  let str = block_device_to_string size blk in
  let* m = Owi.Parse.Binary.Module.from_string str in
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
  let* malloc, env = load_func_from_module link_state None "malloc" in
  let[@warning "-8"] Ok [ V.I32 path_addr ] = Interpret.Concrete.exec_vfunc_from_outside ~locals:[ V.I32 8l (* "file:db\000" *) ] ~env:env
    ~envs:link_state.envs malloc in
  let[@warning "-8"] Ok [ V.I32 sqlite_addr ] = Interpret.Concrete.exec_vfunc_from_outside ~locals:[ V.I32 8l ] ~env:env
    ~envs:link_state.envs malloc in
  let* memory, _env = load_memory_from_module link_state None "my_memory" in
  ignore (Concrete_memory.blit_string memory "file:db\000" ~src:0l ~dst:path_addr ~len:8l);
  let* ptr = Concrete_memory.load_64 memory sqlite_addr in
  Fmt.epr ">>> %Lx\n%!" ptr;
  let* sqlite3_open, env = load_func_from_module link_state None "sqlite3_open" in
  let[@warning "-8"] Ok [ V.I32 _res ] = Interpret.Concrete.exec_vfunc_from_outside ~locals:[ V.I32 path_addr; I32 sqlite_addr ] ~env:env
    ~envs:link_state.envs sqlite3_open in
  (* struct sqlite3 *v;
     sqlite3_open(..., &v); *)
  let* ptr = Concrete_memory.load_64 memory sqlite_addr in
  Fmt.epr ">>> %Lx\n%!" ptr; Ok ()

let run _ size = Miou_solo5.(run [ block "wasm" ]) @@ fun blk () ->
  let rng = Mirage_crypto_rng_miou_solo5.initialize (module Mirage_crypto_rng.Fortuna) in
  let finally () =
    Mirage_crypto_rng_miou_solo5.kill rng in
  Fun.protect ~finally @@ fun () ->
  let result = run_modul ~size blk in
  match result with
  | Error e ->
    let s = Owi.Result.err_to_string e in
    Fmt.pr "error: %s@\n" s;
    assert false
  | Ok _ -> ()

open Cmdliner

let output_options = "OUTPUT OPTIONS"

let verbosity = Logs_cli.level ~docs:output_options ()
let renderer = Fmt_cli.style_renderer ~docs:output_options ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc)

let t0 = Miou_solo5.clock_monotonic ()

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      let t1 = Miou_solo5.clock_monotonic () in
      let delta = Float.of_int (t1 - t0) in
      let delta = delta /. 1_000_000_000. in
      Fmt.kpf k ppf
        ("[+%a][%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue (fmt "%04.04f")) delta
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let setup_logs utf_8 style_renderer level =
  Option.iter (Fmt.set_style_renderer Fmt.stdout) style_renderer;
  Fmt.set_utf_8 Fmt.stdout utf_8;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stdout) ;
  Option.is_none level

let setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)

let size = Arg.(required & opt (some int) None & info [ "s"; "size"])
let term = Term.(const run $ setup_logs $ size)
let info = Cmd.info "unikernel"
let cmd = Cmd.v info term
let () = Cmd.(exit @@ eval cmd)
