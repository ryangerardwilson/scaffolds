#!/bin/bash

# Step 1: Compile modules
ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64 \
  -I utils -I lib utils/renderer.ml

ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64 \
  -I utils -I lib lib/landing.ml

ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64 \
  -I utils -I lib lib/about.ml

ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64 \
  -I utils -I lib lib/login.ml

ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64 \
  -I utils -I lib lib/logout.ml

ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64 \
  -I utils -I lib lib/dashboard.ml

ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64 \
  -I utils -I lib main.ml

# Step 2: Link modules
ocamlfind ocamlc -thread -package cohttp-lwt-unix,dotenv,str,base64 -linkpkg \
  -o app \
  utils/renderer.cmo \
  lib/landing.cmo \
  lib/about.cmo \
  lib/login.cmo \
  lib/logout.cmo \
  lib/dashboard.cmo \
  main.cmo

# Step 3: Clean .cmi, .cmo, .out
find . -type f \( -name "*.cmo" -o -name "*.cmi" -o -name "*.out" \) -exec rm -f {} +

echo "Use the --and_run flag to compile and run the app automatically."

# Step 4: Optionally run
if [[ "$1" == "--and_run" ]]; then
  ./app
fi

