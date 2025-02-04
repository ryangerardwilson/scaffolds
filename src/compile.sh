#!/bin/bash

# Step 1: Compile modules
ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64 \
  -I utils -I lib utils/renderer.ml

ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64,sqlite3 \
  -I utils -I lib utils/migrations.ml

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
  -I utils -I lib lib/assets.ml

ocamlfind ocamlc -c -thread -package cohttp-lwt-unix,dotenv,str,base64 \
  -I utils -I lib main.ml

# Step 2: Link modules
ocamlfind ocamlc -thread -package cohttp-lwt-unix,dotenv,str,base64,sqlite3 -linkpkg \
  -o app \
  utils/renderer.cmo \
  utils/migrations.cmo \
  lib/landing.cmo \
  lib/about.cmo \
  lib/login.cmo \
  lib/logout.cmo \
  lib/dashboard.cmo \
  lib/assets.cmo \
  main.cmo

# Step 3: Clean .cmi, .cmo, .out
find . -type f \( -name "*.cmo" -o -name "*.cmi" -o -name "*.out" \) -exec rm -f {} +

# Step 4: Download the tailwindcss cli, and compile the assets
TAILWIND_CSS_URL="https://github.com/tailwindlabs/tailwindcss/releases/latest/download/tailwindcss-linux-x64"
TAILWIND_CSS_BINARY=".tailwindcss-linux-x64"  # Hidden file
INPUT_FILE=".tailwind_build_input"
OUTPUT_FILE="resources/assets/styles.css"

# Check if the Tailwind CSS binary already exists, download it if not
if [ ! -f "$TAILWIND_CSS_BINARY" ]; then
    curl -sLO "$TAILWIND_CSS_URL"
    mv "tailwindcss-linux-x64" "$TAILWIND_CSS_BINARY"
    chmod +x "$TAILWIND_CSS_BINARY"
fi

./"$TAILWIND_CSS_BINARY" -i "$INPUT_FILE" -o "$OUTPUT_FILE" --minify



echo "[INFO] Use the --and_run flag to compile and run the app automatically."

# Step 4: Optionally run
if [[ "$1" == "--and_run" ]]; then
  ./app
fi

