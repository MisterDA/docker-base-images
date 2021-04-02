open Current.Syntax

module Switch_map = Map.Make(Ocaml_version)

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

let opam_repository os_family =
  let uri, gref =
    let open Uri in
    let uri = of_string (Dockerfile_distro.opam_repository os_family) in
    with_fragment uri None |> to_string, fragment uri in
  Current_git.clone ~schedule:weekly ?gref uri
  |> Current.map Current_git.Commit.id

(* [aliases_of d] gives other tags which should point to [d].
   e.g. just after the Ubuntu 20.04 release, [aliases_of ubuntu-20.04 = [ ubuntu; ubuntu-lts ]] *)
let aliases_of =
  let latest = Dockerfile_distro.latest_distros |> List.map (fun d -> Dockerfile_distro.resolve_alias d, d) in
  fun d -> List.filter_map (fun (d2, alias) -> if d = d2 then Some alias else None) latest

let master_distro = Dockerfile_distro.(resolve_alias master_distro)

type 'a run = ('a, unit, string, Dockerfile.t) format4 -> 'a

let maybe_add_beta (run : 'a run) switch =
  if Ocaml_version.Releases.is_dev switch then
    run "opam repo add beta git://github.com/ocaml/ocaml-beta-repository --set-default"
  else
    Dockerfile.empty

let maybe_add_multicore (run : 'a run) switch =
  if Ocaml_version.Configure_options.is_multicore switch then
    run "opam repo add multicore git://github.com/ocaml-multicore/multicore-opam --set-default"
  else
    Dockerfile.empty

let maybe_install_secondary_compiler (run : 'a run) switch =
  let dune_min_native_support = Ocaml_version.Releases.v4_08 in
  if Ocaml_version.compare switch dune_min_native_support < 0 then
    run "opam install -y ocaml-secondary-compiler"
  else
    Dockerfile.empty

let install_package_archive opam_image =
  let open Dockerfile in
  from ~alias:"archive" opam_image @@
  workdir "/home/opam/opam-repository" @@
  run "opam admin cache --link=/home/opam/opam-repository/cache" @@
  from "alpine:latest" @@
  copy ~chown:"0:0" ~from:"archive" ~src:["/home/opam/opam-repository/cache"] ~dst:"/cache" ()

(* Generate a Dockerfile to install OCaml compiler [switch] in [opam_image]. *)
let install_compiler_df ~os_family ~arch ~switch ?windows_port opam_image =
  let switch_name = Ocaml_version.(with_just_major_and_minor switch |> to_string) in
  let (package_name, package_version) =
    match windows_port with
    | None -> Ocaml_version.Opam.V2.package switch
    | Some port -> Dockerfile_windows.ocaml_for_windows_package_exn ~switch ~arch ~port
  in
  let open Dockerfile in
  let personality = Dockerfile_distro.personality os_family arch in
  let run, run_no_opam, depext, etp_exec =
    let open Dockerfile_windows in
    let bitness = if Ocaml_version.arch_is_32bit arch then "--32" else "--64" in
    match windows_port with
    | None ->
       (fun fmt -> run fmt), (fun fmt -> run fmt), "opam-depext", ["opam"; "exec"; "--"]
    | Some `Msvc ->
       (fun fmt -> run_ocaml_env [bitness; "--ms=vs2019"] fmt),
       (fun fmt -> run_ocaml_env [bitness; "--ms=vs2019"; "--no-opam"] fmt),
       "depext", ["ocaml-env"; "exec"; bitness; "--ms=vs2019"; "--"]
    | Some `Mingw ->
       (fun fmt -> run_ocaml_env [bitness] fmt),
       (fun fmt -> run_ocaml_env [bitness; "--no-opam"] fmt),
       "depext depext-cygwinports", ["ocaml-env"; "exec"; bitness; "--"]
  in
  let shell = maybe (fun pers -> shell [pers; "/bin/sh"; "-c"]) personality in
  let packages =
    let additional_packages = Ocaml_version.Opam.V2.additional_packages switch in
    String.concat "," (Printf.sprintf "%s.%s" package_name package_version :: additional_packages)
  in
  from opam_image @@
  shell @@
  maybe_add_beta run switch @@
  maybe_add_multicore run switch @@
  env ["OPAMYES", "1";
       "OPAMDEPEXTYES", "1"; (* Remove this when https://github.com/ocaml/opam/pull/4563 is merged *)
       "OPAMUNSAFEDEPEXTYES", "1";
       "OPAMERRLOGLEN", "0"; (* Show the whole log if it fails *)
       "OPAMPRECISETRACKING", "1"; (* Mitigate https://github.com/ocaml/opam/issues/3997 *)
      ] @@
  run_no_opam "opam switch create %s --packages=%s" switch_name packages @@
  run "opam install -y %s" depext @@
  run "opam pin add -k version %s %s" package_name package_version @@
  maybe_install_secondary_compiler run switch @@
  entrypoint_exec (Option.to_list personality @ etp_exec) @@
  match os_family with `Linux | `Cygwin -> cmd "bash" | `Windows -> cmd_exec ["cmd.exe"] @@
  copy ~src:["Dockerfile"] ~dst:"/Dockerfile.ocaml" ()

let or_die = function
  | Ok x -> x
  | Error (`Msg m) -> failwith m

(* Pipeline to build the opam base image and the compiler images for a particular architecture. *)
module Arch = struct
  (* 2020-04-29: On Windows, squashing images is still experimental (broken). *)
  let squash = Sys.unix

  let install_opam ~arch ~ocluster ~distro ~opam_repository ~push_target =
    let arch_name = Ocaml_version.string_of_arch arch in
    let os_family = Dockerfile_distro.os_family_of_distro distro in
    let dockerfile =
      `Contents (
        let opam = snd @@ Dockerfile_opam.gen_opam2_distro ~arch ~clone_opam_repo:false distro in
        let open Dockerfile in
        string_of_t (
          opam
          @@ begin match os_family with
             | `Cygwin | `Linux ->
                copy ~chown:"opam:opam" ~src:["."] ~dst:"/home/opam/opam-repository" ()
                @@ run "opam-sandbox-disable"
                @@ run "opam init -k local -a /home/opam/opam-repository --bare"
                @@ run "rm -rf .opam/repo/default/.git"
             | `Windows ->
                let opam_repo = Dockerfile_windows.Cygwin.default.root ^ {|\home\opam\opam-repository|} in
                let opam_root = {|C:\opam\.opam|} in
                copy ~src:["."] ~dst:opam_repo ()
                @@ env [("OPAMROOT", opam_root)]
                @@ run "opam init -k local -a \"%s\" --bare --disable-sandboxing" opam_repo
                @@ Dockerfile_windows.Cygwin.run_sh "rm -rf /cygdrive/c/opam/.opam/repo/default/.git"
             end
          @@ copy ~src:["Dockerfile"] ~dst:"/Dockerfile.opam" ()
        )
      )
    in
    let distro_tag = Dockerfile_distro.tag_of_distro distro in
    Current.component "%s@,%s" distro_tag arch_name |>
    let> opam_repository = opam_repository in
    let options = { Cluster_api.Docker.Spec.defaults with squash; include_git = true } in
    let cache_hint = Printf.sprintf "opam-%s" distro_tag in
    Current_ocluster.Raw.build_and_push ocluster ~src:[opam_repository] dockerfile
      ~cache_hint
      ~options
      ~push_target
      ~pool:(Conf.pool_name os_family arch)

  let install_compiler ~os_family ~arch ~ocluster ~switch ~push_target ?windows_port base =
    let arch_name = Ocaml_version.string_of_arch arch in
    Current.component "%s/%s" (Ocaml_version.to_string switch) arch_name |>
    let> base = base in
    let dockerfile = `Contents (install_compiler_df ~os_family ~arch ~switch ?windows_port base |> Dockerfile.string_of_t) in
    (* ([include_git] doesn't do anything here, but it saves rebuilding during the upgrade) *)
    let options = { Cluster_api.Docker.Spec.defaults with squash; include_git = true } in
    let cache_hint = Printf.sprintf "%s-%s-%s" (Ocaml_version.to_string switch) arch_name base in
    Current_ocluster.Raw.build_and_push ocluster ~src:[] dockerfile
      ~cache_hint
      ~options
      ~push_target
      ~pool:(Conf.pool_name os_family arch)

  let collect_archive ~os_family ~ocluster ~push_target base =
    Current.component "archive" |>
    let> base = base in
    let dockerfile = `Contents (install_package_archive base |> Dockerfile.string_of_t) in
    let options = { Cluster_api.Docker.Spec.defaults with squash; include_git = true } in
    let cache_hint = Printf.sprintf "archive-%s" base in
    Current_ocluster.Raw.build_and_push ocluster ~src:[] dockerfile
      ~cache_hint
      ~options
      ~push_target
      ~pool:(Conf.pool_name os_family `X86_64)

  (* Build the base image for [distro], plus an image for each compiler version. *)
  let pipeline ~ocluster ~opam_repository ~distro arch =
    let os_family = Dockerfile_distro.os_family_of_distro distro in
    let opam_image =
      let push_target =
        Tag.v distro ~arch
        |> Cluster_api.Docker.Image_id.of_string
        |> or_die
      in
      install_opam ~arch ~ocluster ~distro ~opam_repository ~push_target
    in
    let compiler_images =
      Conf.switches ~arch ~distro |> List.map @@ fun switch ->
      let push_target =
        Tag.v distro ~switch ~arch
        |> Cluster_api.Docker.Image_id.of_string
        |> or_die
      in
      let windows_port = match distro with  `Windows (port, _) -> Some port | _ -> None in
      let repo_id = install_compiler ~os_family ~arch ~ocluster ~switch ~push_target ?windows_port opam_image in
      (switch, repo_id)
    in
    (* Build the archive image for the debian 10 / x86_64 image only *)
    let archive_image =
      if distro = Dockerfile_distro.(master_distro |> resolve_alias) && arch = `X86_64 then
        let push_target =
          Tag.archive ~staging:true ()
          |> Cluster_api.Docker.Image_id.of_string
          |> or_die
        in
        Some (collect_archive ~os_family ~ocluster ~push_target opam_image)
      else None
    in
    let compiler_images = Switch_map.of_seq (List.to_seq compiler_images) in
    (opam_image, compiler_images, archive_image)
end

let all_switches arches =
  let module Switch_set = Set.Make(Ocaml_version) in
  arches |> ListLabels.fold_left ~init:Switch_set.empty ~f:(fun acc map ->
      Switch_map.fold (fun k _v acc -> Switch_set.add k acc) map acc
    )
  |> Switch_set.elements

let notify_status ?channel x =
  match channel with
  | None -> x
  | Some channel ->
    let s =
      let+ state = Current.catch x in
      Fmt.str "docker-base-images status: %a" (Current_term.Output.pp Current.Unit.pp) state
    in
    Current.all [
      Current_slack.post channel ~key:"base-images-status" s;
      x   (* If [x] fails, the whole pipeline should fail too. *)
    ]

let label l t =
  Current.component "%s" l |>
  let> v = t in
  Current.Primitive.const v

let linux_pipeline ~ocluster distro =
  let distro_label = Dockerfile_distro.tag_of_distro distro in
  let repo =
    let os_family = Dockerfile_distro.os_family_of_distro distro in
    label distro_label (opam_repository os_family) in
  Current.collapse ~key:"distro" ~value:distro_label ~input:repo @@
    let opam_images, ocaml_images, archive_image =
      let arch_results =
        let arches = Conf.arches_for ~distro in
        List.map (Arch.pipeline ~ocluster ~opam_repository:repo ~distro) arches in
      List.fold_left (fun (aa,ba,ca) (a,b,c) ->
        let ca = match ca,c with Some v, _ -> Some v | None, v -> v in
        a::aa, b::ba, ca) ([], [], None) arch_results in
    let ocaml_images =
      all_switches ocaml_images |> List.filter_map @@ fun switch ->
      let images = List.filter_map (Switch_map.find_opt switch) ocaml_images in
      if images = [] then None
      else (
        let full_tag = Tag.v distro ~switch in
        let tags =
          let distro_aliases = aliases_of distro in
          (* Push the image as e.g. debian-10-ocaml-4.09 and debian-ocaml-4.09 *)
          let tags = full_tag :: List.map (Tag.v ~switch) distro_aliases in
          if switch <> Ocaml_version.Releases.latest then tags
          else (
            (* For every distro, also create a link to the latest OCaml compiler.
               e.g. debian-9 -> debian-9-ocaml-4.09 *)
            let tags = Tag.v_alias distro :: tags in
            (* If [distro] is the latest version of that distribution, make an alias like
               debian -> debian-10-ocaml-4.09 *)
            let tags = List.map Tag.v_alias distro_aliases @ tags in
            (* The top-level alias: latest -> debian-10-ocaml-4.09 *)
            if distro <> master_distro then tags
            else Tag.latest :: tags
          )
        in
        (* Fmt.pr "Aliases: %s -> %a@." full_tag Fmt.(Dump.list string) (List.sort String.compare tags); *)
        let pushes = List.map (fun tag -> Current_docker.push_manifest ?auth:Conf.auth ~tag images |> Current.ignore_value) tags in
        Some (full_tag, Current.all pushes)
      )
    in
    let archive_images =
      match archive_image with
      | None -> []
      | Some image -> [ "archive", Current_docker.push_manifest ?auth:Conf.auth ~tag:(Tag.archive ()) [image] |> Current.ignore_value ]
    in
    Current.all_labelled (
     ("base", Current_docker.push_manifest ?auth:Conf.auth ~tag:(Tag.v distro) opam_images |> Current.ignore_value)
      :: ocaml_images
      @ archive_images)

let windows_distro_pipeline ~ocluster repo distro_label distro_versions =
  (* let multiarch_images = ref [] in *)
  let distro_pipeline multiarches distro =
    let opam_images, ocaml_images, archive_image =
      let arch_results =
        let arches = Conf.arches_for ~distro in
        List.map (Arch.pipeline ~ocluster ~opam_repository:repo ~distro) arches in
      List.fold_left (fun (aa,ba,ca) (a,b,c) ->
        let ca = match ca,c with Some v, _ -> Some v | None, v -> v in
        a::aa, b::ba, ca) ([], [], None) arch_results in
    let (multiarch_images, ocaml_images) =
      all_switches ocaml_images |> List.filter_map @@ begin fun switch ->
        let images = List.filter_map (Switch_map.find_opt switch) ocaml_images in
        if images = [] then None
        else (
          let full_tag = Tag.v distro ~switch in
          let tags =
            (* Push the image as e.g. windows-mingw-20H2-ocaml-4.09  *)
            let tags = [full_tag] in
            if switch <> Ocaml_version.Releases.latest then tags
            else (
              (* For every distro, also create a link to the latest OCaml compiler.
                 e.g. windows-mingw-20H2 -> windows-mingw-20H2-ocaml-4.09 *)
              Tag.v_alias distro :: tags
            )
          in
          (* Fmt.pr "Aliases: %s -> %a@." full_tag Fmt.(Dump.list string) (List.sort String.compare tags); *)
          let pushes = List.map (fun tag -> Current_docker.push_manifest ?auth:Conf.auth ~tag images |> Current.ignore_value) tags in
          Some (Switch_map.add switch images Switch_map.empty, (full_tag, Current.all pushes))
        )
      end |> List.split
    in
    let archive_images =
      match archive_image with
      | None -> []
      | Some image -> [ "archive", Current_docker.push_manifest ?auth:Conf.auth ~tag:(Tag.archive ()) [image] |> Current.ignore_value ]
    in
    let pipeline =
      Current.all_labelled (
          ("base", Current_docker.push_manifest ?auth:Conf.auth ~tag:(Tag.v distro) opam_images |> Current.ignore_value)
          :: ocaml_images
          @ archive_images)
    in
    let multiarch_images =
      let update images = function None -> Some images | Some images' -> Some (images @ images') in (* FIXME: duplicates? *)
      let fold switch images acc = Switch_map.update switch (update images) acc in
      let fold_left acc images = Switch_map.fold fold acc images in
      List.fold_left fold_left multiarches multiarch_images
    in
    (multiarch_images, pipeline)
  in
  let multiarch, pipelines = List.fold_left_map distro_pipeline Switch_map.empty distro_versions in
  let pushes =
    Switch_map.fold (fun switch images pushes ->
        let full_tag = Fmt.str "%s:%s-ocaml-%s" Conf.public_repo distro_label (Tag.tag_of_compiler switch) in
        let tags =
          let tags = [full_tag] in
          if switch <> Ocaml_version.Releases.latest then tags
          else Fmt.str "%s:%s" Conf.public_repo distro_label :: tags
        in
        (* Fmt.pr "Aliases: %s -> %a@." full_tag Fmt.(Dump.list string) (List.sort String.compare tags); *)
        let pushes' = List.map (fun tag -> Current_docker.push_manifest ?auth:Conf.auth ~tag images |> Current.ignore_value) tags in
        (full_tag, Current.all pushes') :: pushes)
      multiarch []
  in
  Current.(collapse ~key:"distro" ~value:distro_label ~input:repo (all ((all_labelled pushes) :: pipelines)))

let windows_pipeline ~ocluster distros =
  let mingw, msvc, cygwin = List.fold_left (fun (mingw, msvc, cygwin) d ->
    match d with
    | `Windows (`Mingw, _) -> (d :: mingw, msvc, cygwin)
    | `Windows (`Msvc, _) -> (mingw, d :: msvc, cygwin)
    | `Cygwin _ -> (mingw, msvc, d :: cygwin)
    | _ -> assert false) ([], [], []) distros
  in
  List.( (filter_map (fun (os_family, distro_label, distros) ->
      match distros with
      | [] -> None
      | distro_versions ->
         let repo = label distro_label (opam_repository os_family) in
         windows_distro_pipeline ~ocluster repo distro_label distro_versions |> Option.some)
    [(`Windows, "windows-mingw", mingw); (`Windows, "windows-msvc", msvc); (`Cygwin, "cygwin", cygwin)]))

(* The main pipeline. Builds images for all supported distribution, compiler version and architecture combinations. *)
let v ?channel ~ocluster () =
  Current.all (
    let linux, windows = Conf.distros |> List.partition @@ fun d ->
      match Dockerfile_distro.os_family_of_distro d with
      | `Linux -> true | `Windows | `Cygwin -> false
    in
    List.map (linux_pipeline ~ocluster) linux @ windows_pipeline ~ocluster windows
    )
  |> notify_status ?channel
