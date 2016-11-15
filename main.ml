open Spotlib.Spot
open Utils
open List

let data_dir = ref "out"
let rev_anon_args = ref []

let opt_data_dir = "--data-dir", Arg.String (fun s -> data_dir := s), "data dir (default \"./out\")"  

let opt_src_dir =
  "--src-dir",
  Arg.String (fun s ->
    if not & File.Test._d s then failwithf "%s specified by --src-dir is not a directory" s;
    Cm.non_opam_dirs +::= s),
  "src dir out of OPAM"  

module type SubCommand = sig
  val name : string
  val summary : string
  val exec : unit -> unit
end

module Separator = struct
  let name = ""
  let summary = ""
  let exec () = assert false
end
  
module Scrape = struct
  let name = "scrape"
  let summary = "Scrape OCamlFind packages"
  let exec () =
    let dump = ref false in
    Arg.parse 
      [ opt_data_dir
      ; opt_src_dir
      ; "--dump-humps", Arg.Set dump, "Dump humps"
      ]
      (fun x -> rev_anon_args +::= x)
    & String.concat "" [ !% "%s scrape [ options ] [ packages ]\n" command_name
                       ; !% "  Scrape given OCamlFind packages.\n"
                       ; !% "  If no packages are specified, all the available packages are scraped.\n"
                       ];
    let anon_args = rev !rev_anon_args in
    Scrape.packages !data_dir anon_args
end

module Test = struct
  let name = "test"
  let summary = "Scrape given files then print"

  let exec () =
    Cm.test_mode := true; (* make Cm.guess not loading things on the fly *)
    Humpext.test := true; (* XXX bad code *)
    Arg.parse []
      (fun x -> rev_anon_args +::= x)
    & String.concat "" [ !% "%s test [ options ] [ files ]\n" command_name
                       ; !% "  Scrape given files and print the results without saving.\n"
                       ];
    let anon_args = rev !rev_anon_args in
    flip iter anon_args & fun a ->
      match Filename.split_extension a with
      | _, ".cmi" -> Sigscrape.test_cmi a
      | _, (".cmt" | ".cmti") ->
          let m = module_name a in
          let h = Humpscrape.test_cmt a in
          (* XXX "NOTOP" everywhere *) 
          let f = Humpflat.flatten (Outcometree.(Oide_dot (Oide_ident "NOTOP", m))) h in
          !!% "Flat =>@.%a@."
            (Ocaml.format_with [%derive.ocaml_of: Humpflat.ent list]) f
      | _ -> assert false
end

module Link = struct
  let name = "link"
  let summary = "Link *.dat files"
  let exec () =
    Arg.parse 
      [ opt_data_dir
      ]
      (fun _ -> failwith "link subcommand does not take arguments")
    & String.concat "" [ !% "%s link [ options ]\n" command_name
                       ; !% "  Link data files.\n"
                       ];
    ignore & Datalink.link_db !data_dir
end

module Dumpdb = struct
  let name = "dumpdb"
  let summary = "Dump the final data base"
  let exec () =
    Arg.parse 
      [ opt_data_dir
      ]
      (fun _ -> failwith "dumpdb does not take arguments")
    & String.concat "" [ !% "%s dumpdb [ options ]\n" command_name
                       ; !% "  Dump all the search db items.  The output may be very huge.\n"
                       ];
    Search.dump !data_dir
end

module Search = struct
  let name = "search"
  let summary = "Start interactive search session"
  let exec () =
    Arg.parse 
      [ opt_data_dir
      ]
      (fun _ -> failwith "search subcommand does not take arguments")
    & String.concat "" [ !% "%s search [ options ]\n" command_name
                       ; !% "  Interactively search scraped data.\n"
                       ];
    Search.do_search !data_dir
end

module Package = struct
  let name = "package"
  let summary = "Show OCamlFind package information"
  let exec () =
    Arg.parse []
      (fun x -> rev_anon_args +::= x)
    & String.concat "" [ !% "%s package [ packages ]\n" command_name
                       ; !% "  Show OCamlFind package information.\n"
                       ; !% "  If no packages are specified, all the available packages are printed.\n"
                       ];
    let anon_args = rev !rev_anon_args in
    let open Opamfind in
    let ps = !!Package.opams_of_ocamlfind in
    let ps = match anon_args with
      | [] -> ps
      | _ -> flip filter ps & fun (ag,_) -> mem ag.Ocamlfind.Analyzed_group.name anon_args
    in
    !!% "@[%a@]@."
      (Ocaml.format_with [%derive.ocaml_of: (Ocamlfind.Analyzed_group.t * Opam.Package.t list) list] ) ps
end

module Dumphump = struct
  let name = "dumphump"
  let summary = "Dump hump data"
  let exec () =
    let rev_anon_args = ref [] in
    Arg.parse [ opt_data_dir
              ]
      (fun x -> rev_anon_args +::= x)
    & String.concat "" [ !% "%s dumphump [ packages ]\n" command_name
                       ; !% "  Dump hump data.\n"
                       ; !% "  If no packages are specified, stdlib is dumped.\n"
                       ];
    let anon_args = rev !rev_anon_args in
    let packs = match anon_args with [] -> ["stdlib"] | _ -> anon_args in
    let xs = map (Humpflat.load_package ~datadir:!data_dir) packs in
    !!% "%a@." (Ocaml.format_with [%derive.ocaml_of: (Hump.path * Hump.expr) list list]) xs
end

module Evaltest = struct
  let name = "evaltest"
  let summary = "Eval package humps and print the result"
  let exec () =
    let rev_anon_args = ref [] in
    Arg.parse [ opt_data_dir
              ]
      (fun x -> rev_anon_args +::= x)
    & String.concat "" [ !% "%s evaltest [ packages ]\n" command_name
                       ; !% "  Test eval package humps.\n"
                       ; !% "  If no packages are specified, stdlib is tested.\n"
                       ];
    let anon_args = rev !rev_anon_args in
    let packs = match anon_args with [] -> ["stdlib"] | _ -> anon_args in
    let eval = Humpflat.eval_package ~datadir:!data_dir in
    flip iter packs & fun p ->
      !!% "Eval %s...@." p;
      let res = eval p in
      !!% "Eval %s done.@." p;
      flip iter res & fun (p,e) ->
        !!% "%a@." (Ocaml.format_with [%derive.ocaml_of: Hump.path]) p;
        !!% "%a@." (Ocaml.format_with [%derive.ocaml_of: Hump.expr]) e
end

module Flattest = struct

  let name = "flattest"

  let summary = "Print flattened humps"

  let exec () =
    let rev_anon_args = ref [] in
    Arg.parse [ opt_data_dir ]
      (fun x -> rev_anon_args +::= x)
    & String.concat "" [ !% "%s flattest [ packages ]\n" command_name
                       ; !% "  Print flattened humps.\n"
                       ; !% "  If no packages are specified, stdlib is tested.\n"
                       ];
    let anon_args = rev !rev_anon_args in
    let packs = match anon_args with [] -> ["stdlib"] | _ -> anon_args in
    Humpflat.verbose := true;
    iter (ignore *< Humpflat.flatten_package ~datadir:!data_dir) packs;
    Humpflat.verbose := false

end

module Cmtest = struct
  let name = "cmtest"
  let summary = "Print the reachable modules of packages"
  let exec () =
    let rev_anon_args = ref [] in
    Arg.parse [ opt_src_dir
              ]
      (fun x -> rev_anon_args +::= x)
    & String.concat "" [ !% "%s cmtest [ packages ]\n" command_name
                       ; !% "  Print the reachable modules of packages.\n"
                       ];
    let anon_args = rev !rev_anon_args in
    Cm.test anon_args
end

let subcommands : (module SubCommand) list =
  [ (module Scrape)
  ; (module Link)
  ; (module Search)
  ; (module Separator)
  ; (module Package)
  ; (module Test)
  ; (module Dumpdb)
  ; (module Dumphump)
  ; (module Evaltest)
  ; (module Flattest)
  ; (module Cmtest)
  ]

let () =
  let subcommands =
    map (fun m ->
      let module M = (val m : SubCommand) in
      M.name, (M.exec, M.summary)) subcommands
  in
  let subcommands_help = map (fun (n,(_,s)) ->
    if n = "" then "\n"
    else !%"\t%s %s [ options ]\t: %s\n" command_name n s
  ) subcommands
  in
  let subcommands = filter (fun (n, _) -> n <> "") subcommands in
  let global_help =
    String.concat ""
      & [ !%"NAME\n"
        ; !%"\t%s - OCamlOScope2 command line interface\n\n" command_name
        ; !%"SYNOPSIS\n"
        ] 
      @ subcommands_help
      @ [ !%"\n"
        ; !%"  For help of each subcommand, %s <command> --help\n" command_name
        ]
  in
  let print_global_help_and_exit () = print_string global_help; exit 1 in
  if Array.length Sys.argv <= 1 then print_global_help_and_exit ()
  else begin
    incr Arg.current;
    match assoc_opt Sys.argv.(1) subcommands with
    | Some (f,_) -> f ()
    | None -> print_global_help_and_exit ();
  end

