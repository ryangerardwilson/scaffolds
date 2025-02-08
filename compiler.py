#!/usr/bin/env python3
import os
import sys
import subprocess
import shutil
import json
import re
from packaging.version import parse as parse_version


MAJOR_RELEASE_NUMBER = 2
OCAML_PACKAGES = ""  # A comma separated string of packages injected into the compile and link commands

###############################################################################
# STEP I: Preprocessing – Generate lib/templates_lib.ml and lib/scaffolder_lib.ml
###############################################################################


def get_versions(MAJOR_RELEASE_NUMBER=None):
    """
    Retrieves the current version number from the server and computes a new version.

    Steps:
      1) Reads ~/.rgwfuncsrc to obtain SSH credentials for the preset 'icdattcwsm'.
      2) SSH into the server and list the .deb files in:
         /home/rgw/Apps/frontend-sites/files.ryangerardwilson.com/scaffolds/debian/dists/stable/main/binary-amd64
      3) Takes the last .deb filename (expected to be of the form:
         scaffolds_<MAJOR>.<MINOR>.<PATCH>-<REV>.deb) and extracts the version string.
      4) If MAJOR_RELEASE_NUMBER is None (or matches the current major):
           - Returns a new version having the same MAJOR and MINOR with PATCH incremented by 1
         Otherwise:
           - Returns a new version of the form: {MAJOR_RELEASE_NUMBER}.0.1-1
    Returns:
         (current_version, new_version)
         where current_version is in the form "X.Y.Z-R" and new_version similarly.
    """
    # Step 1: Read the config file for SSH credentials
    config_path = os.path.expanduser("~/.rgwfuncsrc")
    if not os.path.exists(config_path):
        raise FileNotFoundError(f"Cannot find config file: {config_path}")

    with open(config_path, "r", encoding='utf-8') as f:
        data = json.load(f)

    vm_presets = data.get("vm_presets", [])
    preset = next((p for p in vm_presets if p.get("name") == "icdattcwsm"), None)
    if not preset:
        raise ValueError("No preset named 'icdattcwsm' found in ~/.rgwfuncsrc")

    host = preset["host"]
    ssh_user = preset["ssh_user"]
    ssh_key_path = preset["ssh_key_path"]

    # Step 2: List .deb files on the server
    remote_deb_dir = (
        "/home/rgw/Apps/frontend-sites/files.ryangerardwilson.com/scaffolds/debian/"
        "dists/stable/main/binary-amd64"
    )
    ssh_cmd = (
        f"ssh -i {ssh_key_path} {ssh_user}@{host} "
        f"\"ls -1 {remote_deb_dir}/*.deb 2>/dev/null || true\""
    )
    output = subprocess.check_output(ssh_cmd, shell=True).decode("utf-8").strip()

    if not output:
        raise FileNotFoundError(f"No .deb files found in {remote_deb_dir}")

    # Assume the last line is the relevant .deb file
    deb_file_path = output.split("\n")[-1].strip()

    # Step 3: Parse the filename for version info.
    # Expected filename format: scaffolds_<MAJOR>.<MINOR>.<PATCH>-<REV>.deb
    filename = os.path.basename(deb_file_path)
    match = re.match(r"^scaffolds_(\d+\.\d+\.\d+)-(\d+)\.deb$", filename)
    if not match:
        raise ValueError(f"Could not parse version from deb file name: {filename}")

    version_str = match.group(1)    # e.g. "1.0.3"
    revision_str = match.group(2)   # e.g. "1"
    current_version = f"{version_str}-{revision_str}"

    # Split version_str into major, minor, patch components.
    major_str, minor_str, patch_str = version_str.split(".")
    server_major = int(major_str)
    server_minor = int(minor_str)
    server_patch = int(patch_str)
    server_revision = int(revision_str)

    # Step 4: Compute new version string.
    if MAJOR_RELEASE_NUMBER is None:
        # No new major specified; assume we want same major/minor with patch +1
        new_major = server_major
        new_minor = server_minor
        new_patch = server_patch + 1
        new_revision = server_revision  # keeping the same revision, or adjust if necessary
    else:
        user_major = int(MAJOR_RELEASE_NUMBER)
        if user_major == server_major:
            # Same major: increment patch (or adjust this logic if desired)
            new_major = server_major
            new_minor = server_minor
            new_patch = server_patch + 1
            new_revision = server_revision
        else:
            # New major version requested, format as: {MAJOR_RELEASE_NUMBER}.0.1-1
            new_major = user_major
            new_minor = 0
            new_patch = 1
            new_revision = 1

    new_version = f"{new_major}.{new_minor}.{new_patch}-{new_revision}"
    return current_version, new_version


def get_new_version_number(MAJOR_RELEASE_NUMBER=None):
    """
    1) Reads ~/.rgwfuncsrc JSON to obtain the server credentials preset named 'icdattcwsm'.
    2) SSH into the server and find the current .deb file in:
       /home/rgw/Apps/frontend-sites/files.ryangerardwilson.com/scaffolds/debian/dists/stable/main/binary-amd64
    3) Parse the .deb file name to extract the current version string (MAJOR.MINOR.PATCH-REV).
    4) If MAJOR_RELEASE_NUMBER is None or matches the current MAJOR, return a version
       with PATCH incremented by 1. Otherwise, return a version of the form:
       {MAJOR_RELEASE_NUMBER}.0.1-1
    """

    # -------------------------------------------------------------------------
    # Step 1) Read ~/.rgwfuncsrc and find the 'icdattcwsm' preset
    # -------------------------------------------------------------------------
    funcs_path = os.path.expanduser("~/.rgwfuncsrc")
    if not os.path.exists(funcs_path):
        raise FileNotFoundError(f"Cannot find config file: {funcs_path}")

    with open(funcs_path, "r") as f:
        data = json.load(f)

    vm_presets = data.get("vm_presets", [])
    preset = next((p for p in vm_presets if p.get("name") == "icdattcwsm"), None)
    if not preset:
        raise ValueError("No preset named 'icdattcwsm' found in ~/.rgwfuncs")

    host = preset["host"]
    ssh_user = preset["ssh_user"]
    ssh_key_path = preset["ssh_key_path"]

    # -------------------------------------------------------------------------
    # Step 2) SSH to the server to locate the current .deb file
    # -------------------------------------------------------------------------
    remote_deb_dir = (
        "/home/rgw/Apps/frontend-sites/files.ryangerardwilson.com/scaffolds/debian/"
        "dists/stable/main/binary-amd64"
    )

    # List all .deb files
    ssh_cmd = (
        f"ssh -i {ssh_key_path} {ssh_user}@{host} "
        f"\"ls -1 {remote_deb_dir}/*.deb 2>/dev/null || true\""
    )
    output = subprocess.check_output(ssh_cmd, shell=True).decode().strip()

    # If no .deb files exist, raise an error (or handle how you prefer)
    if not output:
        raise FileNotFoundError(
            f"No .deb files found in {remote_deb_dir}"
        )

    # For simplicity, assume there's only one relevant .deb file, or
    # take the last line if multiple. Adjust logic as needed.
    deb_file_path = output.split("\n")[-1].strip()
    # Example: /home/rgw/.../binary-amd64/scaffolds_1.0.3-1.deb

    # -------------------------------------------------------------------------
    # Step 3) Parse the .deb file name to extract the version
    #         We expect a name of the form: scaffolds_X.Y.Z-REV.deb
    # -------------------------------------------------------------------------
    filename = os.path.basename(deb_file_path)  # scaffolds_1.0.3-1.deb
    match = re.match(r"^scaffolds_(\d+\.\d+\.\d+)-(\d+)\.deb$", filename)
    if not match:
        raise ValueError(
            f"Could not parse version from deb file name: {filename}"
        )

    version_str = match.group(1)  # "1.0.3"
    revision_str = match.group(2)  # "1"

    # Split the version_str into major, minor, patch
    major_str, minor_str, patch_str = version_str.split(".")
    server_major = int(major_str)    # 1
    server_minor = int(minor_str)    # 0
    server_patch = int(patch_str)    # 3
    server_revision = int(revision_str)  # 1

    # -------------------------------------------------------------------------
    # Step 4) Compute new version based on MAJOR_RELEASE_NUMBER
    # -------------------------------------------------------------------------
    if MAJOR_RELEASE_NUMBER is None:
        # If user didn't specify a new major, we assume they
        # want to increment the patch of the existing major.
        new_major = server_major
        new_minor = server_minor
        new_patch = server_patch + 1
        new_revision = server_revision  # keep the same revision
    else:
        # Convert user input to int
        user_major = int(MAJOR_RELEASE_NUMBER)

        if user_major == server_major:
            # If the requested major is the same as the server major,
            # just increment patch
            new_major = server_major
            new_minor = server_minor
            new_patch = server_patch + 1
            new_revision = server_revision
        else:
            # Otherwise, create a new major version: MAJOR.0.1-1
            new_major = user_major
            new_minor = 0
            new_patch = 1
            new_revision = 1

    # Construct the new version string
    new_version_str = f"{new_major}.{new_minor}.{new_patch}-{new_revision}"

    return new_version_str


def replace_version(main_go_path, new_version):
    """
    Reads the Go source file at main_go_path and replaces the value of the 'version'
    variable with new_version. It assumes a line like:
       version := "old_value"
    This function uses a regular expression to substitute the value.
    """
    with open(main_go_path, 'r', encoding='utf-8') as f:
        content = f.read()

    pattern = r'(version\s*:=\s*")[^"]*(")'

    def replacer(match):
        # match.group(1) is the part before the version value (including the opening quote)
        # match.group(2) is the closing quote.
        return match.group(1) + new_version + match.group(2)

    new_content = re.sub(pattern, replacer, content)

    with open(main_go_path, 'w', encoding='utf-8') as f:
        f.write(new_content)

    print(f"[INFO] Replaced version with {new_version} in {main_go_path}")


def build_variable_name(root_relative_path):
    """
    Given a path relative to 'src', build a variable name.
    Rules:
      - For a file in the root of src (like "main.ml") → "file_main_ext_ml"
      - For a file like ".env" → "ext_env"
      - For files in subdirectories, include 'dir_<dirname>' parts.
    """
    parts = root_relative_path.split(os.sep)
    dirs = parts[:-1]
    filename = parts[-1]
    if filename.startswith('.'):  # pure extension, e.g. ".env"
        var_name = f"ext_{filename[1:]}" if len(filename) > 1 else "ext_"
    else:
        if '.' in filename:
            name, ext = filename.rsplit('.', 1)
        else:
            name, ext = filename, ""
        if not dirs:
            if ext == "":
                var_name = f"file_{name}"
            else:
                var_name = f"file_{name}_ext_{ext}"
        else:
            dir_chunks = [f"dir_{d}" for d in dirs]
            if ext == "":
                var_name = "_".join(dir_chunks + [f"file_{name}"])
            else:
                var_name = "_".join(dir_chunks + [f"file_{name}", f"ext_{ext}"])
    # Replace any dashes or spaces with underscore
    return var_name.replace('-', '_').replace(' ', '_')


def gather_files_from_src(src_dir):
    """
    Recursively gather files in src_dir.
    Returns a list of tuples: (relative_path, variable_name, file_content).
    """
    collected = []
    for root, dirs, files in os.walk(src_dir):
        for f in files:
            try:
                full_path = os.path.join(root, f)
                rel_path = os.path.relpath(full_path, src_dir)
                # Skip unwanted files (you can customize these filters)
                if rel_path == "app" and '.' not in rel_path:
                    continue
                if '.tailwindcss-linux-x64' in rel_path:
                    continue
                if '.db' in rel_path or '.swp' in rel_path:
                    continue
                var_name = build_variable_name(rel_path)
                with open(full_path, 'r', encoding='utf-8') as infile:
                    content = infile.read()
                collected.append((rel_path, var_name, content))
            except Exception as e:
                print(f"[ERROR] Could not process file {full_path}: {e}")
    return collected


def write_main_go(all_files, version):
    """
    Write a new main.go file that:
      - Imports "embed" and required packages.
      - Embeds every file from src using a //go:embed directive.
      - Defines the Scaffold function and command-line parsing.
    """

    ascii_art = """
  ░▒▓███████▓▒░░▒▓██████▓▒░ ░▒▓██████▓▒░░▒▓████████▓▒░▒▓████████▓▒░▒▓██████▓▒░░▒▓█▓▒░      ░▒▓███████▓▒░ ░▒▓███████▓▒░
 ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
 ░▒▓█▓▒░      ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
  ░▒▓██████▓▒░░▒▓█▓▒░      ░▒▓████████▓▒░▒▓██████▓▒░ ░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░
        ░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░      ░▒▓█▓▒░
        ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░      ░▒▓█▓▒░
 ░▒▓███████▓▒░ ░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░      ░▒▓██████▓▒░░▒▓████████▓▒░▒▓███████▓▒░░▒▓███████▓▒░

         ▗▖   ▄   ▄     ▗▄▄▖ ▄   ▄ ▗▞▀▜▌▄▄▄▄       ▗▄▄▖▗▞▀▚▖ ▄▄▄ ▗▞▀▜▌ ▄▄▄ ▐▌    ▗▖ ▗▖▄ █  ▄▄▄  ▄▄▄  ▄▄▄▄
         ▐▌   █   █     ▐▌ ▐▌█   █ ▝▚▄▟▌█   █     ▐▌   ▐▛▀▀▘█    ▝▚▄▟▌█    ▐▌    ▐▌ ▐▌▄ █ ▀▄▄  █   █ █   █
         ▐▛▀▚▖ ▀▀▀█     ▐▛▀▚▖ ▀▀▀█      █   █     ▐▌▝▜▌▝▚▄▄▖█         █ ▗▞▀▜▌    ▐▌ ▐▌█ █ ▄▄▄▀ ▀▄▄▄▀ █   █
         ▐▙▄▞▘▄   █     ▐▌ ▐▌▄   █                ▝▚▄▞▘                 ▝▚▄▟▌    ▐▙█▟▌█ █
               ▀▀▀            ▀▀▀
    """

    with open("main.go", 'w', encoding='utf-8') as f:
        # Write package and import block.
        f.write("package main\n\n")
        f.write("import (\n")
        f.write("\t\"embed\"\n")
        f.write("\t\"fmt\"\n")
        f.write("\t\"io/ioutil\"\n")
        f.write("\t\"log\"\n")
        f.write("\t\"os\"\n")
        f.write("\t\"path/filepath\"\n")
        f.write("\t\"strings\"\n")
        f.write(")\n\n")
        # Dummy reference to avoid "imported and not used" error.
        f.write("var _ embed.FS\n\n")
        # Write embed directives for each file.
        # (Assumes that the src directory is in the project root.)
        for rel_path, var_name, content in all_files:
            # Use forward slashes in embed paths.
            embed_path = "src/" + rel_path.replace(os.sep, "/")
            f.write(f"//go:embed {embed_path}\n")
            f.write(f"var {var_name} string\n\n")
        # Write helper functions.
        f.write("func ensureFullPath(path string) error {\n")
        f.write("\treturn os.MkdirAll(path, 0755)\n")
        f.write("}\n\n")
        f.write("func writeFile(filename, content string) error {\n")
        f.write("\terr := ioutil.WriteFile(filename, []byte(content), 0644)\n")
        f.write("\tif err != nil { return err }\n")
        f.write("\tfmt.Printf(\"[INFO] scaffolds - Created or updated file: %s\\n\", filename)\n")
        f.write("\treturn nil\n")
        f.write("}\n\n")
        # Write Scaffold function:
        f.write("func Scaffold(targetDir string) error {\n")
        f.write("\tif err := ensureFullPath(targetDir); err != nil { return err }\n")
        f.write("\tfullPath := func(subPath string) string { return filepath.Join(targetDir, subPath) }\n\n")
        # For each embedded file, add directory creation (if needed) and file writing.
        for rel_path, var_name, _ in all_files:
            sub_dir = os.path.dirname(rel_path)
            if sub_dir:
                f.write(f"\tif err := ensureFullPath(fullPath(\"{sub_dir.replace(os.sep, '/')}\")); err != nil {{ return err }}\n")
            f.write(f"\tif err := writeFile(fullPath(\"{rel_path.replace(os.sep, '/')}\"), {var_name}); err != nil {{ return err }}\n\n")
        # Special handling: mark "compiler" as executable.
        f.write("\tcompilerPath := fullPath(\"compiler\")\n")
        f.write("\tif _, err := os.Stat(compilerPath); err == nil {\n")
        f.write("\t\tif err := os.Chmod(compilerPath, 0755); err != nil {\n")
        f.write("\t\t\tlog.Printf(\"[ERROR] scaffolds - Could not set executable permission on compiler: %v\\n\", err)\n")
        f.write("\t\t} else {\n")
        f.write("\t\t\tfmt.Println(\"[INFO] scaffolds - Set +x on compiler\")\n")
        f.write("\t\t}\n")
        f.write("\t}\n\n")
        f.write("\tfmt.Println(\"[INFO] scaffolds - Scaffolding complete. You can now edit your files or compile.\")\n")
        f.write("\treturn nil\n")
        f.write("}\n\n")
        # Write command-line argument parsing function.
        f.write("func getScaffoldDirectory() (string, error) {\n")
        f.write("\targs := os.Args[1:]\n")
        f.write("\tfor i, arg := range args {\n")
        f.write("\t\tif arg == \"--new\" {\n")
        f.write("\t\t\tif i+1 < len(args) {\n")
        f.write("\t\t\t\ttarget := args[i+1]\n")
        f.write("\t\t\t\tif strings.HasPrefix(target, \"--\") {\n")
        f.write("\t\t\t\t\treturn \"\", fmt.Errorf(\"no valid directory specified after '--new'\")\n")
        f.write("\t\t\t\t}\n")
        f.write("\t\t\t\treturn target, nil\n")
        f.write("\t\t\t}\n")
        f.write("\t\t\treturn \"\", fmt.Errorf(\"no argument found after '--new'\")\n")
        f.write("\t\t}\n")
        f.write("\t}\n")
        f.write("\treturn \"\", fmt.Errorf(\"'--new' flag not found\")\n")
        f.write("}\n\n")
        # Then when writing main.go, include the ASCII art inside Go backticks.
        f.write("func main() {\n")
        f.write("\t// Print ASCII art banner\n")
        # Write the raw literal open backtick, then the ASCII art, then the closing backtick.
        f.write("\tconst asciiArt = `\n")
        f.write(ascii_art + "\n")
        f.write("`\n")
        f.write("\tfmt.Println(asciiArt)\n\n")
        f.write("\tversion := \"" + version + "\"\n")
        f.write("\tfmt.Printf(\"Version: %s\\n\", version)\n\n")
        f.write("\ttargetPath, err := getScaffoldDirectory()\n")
        f.write("\tif err != nil {\n")
        f.write("\t\tfmt.Printf(\"Error: %s\\n\", err.Error())\n")
        f.write("\t\tfmt.Println(\"Program requires the name of the new directory where it will scaffold your project after the '--new' flag.\")\n")
        f.write("\t\tos.Exit(1)\n")
        f.write("\t}\n\n")
        f.write("\tif err := Scaffold(targetPath); err != nil {\n")
        f.write("\t\tfmt.Printf(\"Scaffolding failed: %v\\n\", err)\n")
        f.write("\t\tos.Exit(1)\n")
        f.write("\t}\n")
        f.write("}\n")

    print("[INFO] Generated main.go dynamically based on files in src/")


def step1_preprocessing(args, current_version, new_version):

    print("[INFO] Step I - Preprocessing – Generating main.go")
    src_dir = "src"
    if not os.path.isdir(src_dir):
        print(f"[ERROR] src directory not found at {os.path.abspath(src_dir)}")
        sys.exit(1)
    all_files = gather_files_from_src(src_dir)
    if not all_files:
        print("[WARNING] No files found in src/.")
    write_main_go(all_files, current_version)

    if "--publish" in args:
        print("[INFO] Step I - Preprocessing – Adding version number to main.ml")
        # Assume main.ml is in the same directory as this script
        script_dir = os.path.dirname(os.path.abspath(__file__))
        main_go_path = os.path.join(script_dir, 'main.go')

        # Replace the placeholder
        replace_version(main_go_path, new_version)

    print("[INFO] Step I - Preprocessing complete")

################################################################################
# Steps II–V: Compile, Link, Cleanup, and (optionally) generate test scaffold
################################################################################
# These steps basically perform the equivalent of this shell script:
# #!/bin/bash

# # Helper message to inform the user about the flag
# echo "Use the --and_generate_test_scaffold flag to generate the test scaffold."

# # Step I - Compile modules in order of dependencies
# # Although, we can use 'ocamlc -c file_name.ml', when we specify paths in our command we need to use 'ocamlc -c -I lib lib/file_name.ml'
# ocamlc -c -I lib lib/templates_lib.ml
# ocamlc -c -I lib lib/scaffolder_lib.ml
# Include the unix directory in the search path
# ocamlc -c -I lib -I +unix -w +33 lib/templates_lib.ml
# ocamlc -c -I lib -I +unix -w +33 lib/scaffolder_lib.ml


# # Step II - Link modules to main in order of dependencies
# ocamlc -I lib -I +unix -o scaffolds unix.cma lib/templates_lib.cmo lib/scaffolder_lib.cmo main.ml

# # Step III - Remove all .cmo, .cmi, .out files from the pwd and all sub-directories
# find . -type f \( -name "*.cmo" -o -name "*.cmi" -o -name "*.out" \) -exec rm -f {} +

# # Step IV - Check for the --and_generate_test_scaffold flag and execute the scaffold command if present
# if [[ " $@ " =~ " --and_generate_test_scaffold " ]]; then
#   # Remove the test directory if it exists
#   if [ -d "test" ]; then
#     rm -rf test
#     echo "Existing 'test' directory removed."
#   fi
#
#   # Execute the scaffold generation
#   ./scaffolds --new test
#   echo "Test scaffold has been generated."
# fi
##############################################################################

def step2_compile():
    """
    Step II: Compile modules in order of dependencies (native code).

    The 'packages' parameter should be a comma-separated string of packages.
    If it is not empty, the '-package' flag will be added to the command.
    """

    print("[INFO] STEP II - Now building the Go binary...")
    env = os.environ.copy()
    env["GOOS"] = "linux"
    env["GOARCH"] = "amd64"
    env["CGO_ENABLED"] = "0"
    build_cmd = ["go", "build", "-o", "scaffolds", "main.go"]
    subprocess.run(build_cmd, check=True, env=env)
    print("[INFO] Build complete. To generate a scaffold, run: ./scaffolds --new <target_directory>")


def step5_optional_test_scaffold(args):
    """
    Step V: Check for the --test flag, if present:
        - remove 'test' dir if exists
        - run ./scaffolds --new test
    """
    if "--test" in args:
        print("[INFO] Step V - Detected --and_generate_test_scaffold flag.")
        if os.path.isdir("test"):
            print("[INFO] Step V - Removing existing 'test' directory...")
            subprocess.run(["rm", "-rf", "test"], check=True)
        print("[INFO] Step V - Generating test scaffold via ./scaffolds --new test")
        subprocess.run(["./scaffolds", "--new", "test"], check=True)
        print("[INFO] Step VI Completed!")
    else:
        print("[INFO] Use the --test flag to generate the test scaffold.")


################################################################################
# Step VI - Publish Release
################################################################################

def publish_release(version):

    def build_deb(version):
        # 2) Define paths/naming
        build_root = f"debian/version_build_folders/scaffolds_{version}"
        build_debian = os.path.join(build_root, "DEBIAN")
        build_bin_dir = os.path.join(build_root, "usr/local/bin")
        out_debs_dir = "debian/version_debs"

        # 3) Ensure directories exist
        os.makedirs(build_debian, exist_ok=True)
        os.makedirs(build_bin_dir, exist_ok=True)
        os.makedirs(out_debs_dir, exist_ok=True)

        # 4) Copy your existing scaffolds binary into the package structure
        shutil.copy2("scaffolds", os.path.join(build_bin_dir, "scaffolds"))

        # 5) Create the control file
        control_content = f"""Package: scaffolds
Version: {version}
Section: utils
Priority: optional
Architecture: amd64
Maintainer: Ryan Gerard Wilson <ryangerardwilson@gmail.com>
Description: An OCaml-powered web application framework with syntax so pretty, you'd want to marry it!
"""
        control_path = os.path.join(build_debian, "control")
        with open(control_path, "w") as f:
            f.write(control_content)

        # 6) Build the package with dpkg-deb
        output_deb = os.path.join(out_debs_dir, f"scaffolds_{version}.deb")
        cmd = ["dpkg-deb", "--build", build_root, output_deb]
        print(f"[INFO] Running: {' '.join(cmd)}")
        subprocess.check_call(cmd)
        print(f"[INFO] Finished building {output_deb}")

    def prepare_deb_for_distribution(version):
        """
        1) Remove any old 'debian/dists/stable' to avoid stale files.
        2) Re-create 'debian/dists/stable/main/binary-amd64'.
        3) Ensure an overrides.txt with 'scaffolds optional utils'.
        4) Copy scaffolds_{version}.deb into binary-amd64.
        5) Run dpkg-scanpackages to create Packages -> Packages.gz.
        6) Use apt-ftparchive to generate a Release file with checksums.
        7) Sign Release with GPG (optional).
        """
        # 1) Remove old 'stable' directory entirely to avoid hash mismatches
        stable_dir = "debian/dists/stable"
        if os.path.exists(stable_dir):
            print("[INFO] Removing previous debian/dists/stable folder to ensure a clean slate.")
            shutil.rmtree(stable_dir)

        # 2) Re-create apt_binary_dir
        apt_binary_dir = "debian/dists/stable/main/binary-amd64"
        os.makedirs(apt_binary_dir, exist_ok=True)

        # 3) Ensure an overrides.txt file is present
        overrides_path = os.path.join(apt_binary_dir, "overrides.txt")
        if not os.path.exists(overrides_path):
            with open(overrides_path, "w") as f:
                f.write("scaffolds optional utils\n")
        print(f"[INFO] overrides.txt verified at {overrides_path}")

        # 4) Copy the newly built .deb into the apt_binary_dir
        deb_source = os.path.join("debian/version_debs", f"scaffolds_{version}.deb")
        print(f"[INFO] Copying {deb_source} to {apt_binary_dir}")
        shutil.copy2(deb_source, apt_binary_dir)

        # 5) Generate Packages and Packages.gz with dpkg-scanpackages
        pkg_cmd = [
            "dpkg-scanpackages",
            "--multiversion",
            ".",
            "overrides.txt"
        ]
        packages_path = os.path.join(apt_binary_dir, "Packages")
        print("[INFO] Generating Packages via dpkg-scanpackages...")
        with open(packages_path, "w") as pf:
            subprocess.check_call(pkg_cmd, cwd=apt_binary_dir, stdout=pf)
        print(f"[INFO] Created {packages_path}")

        prefix = "dists/stable/main/binary-amd64/"
        print("[INFO] Adjusting 'Filename:' entries to remove './'...")
        with open(packages_path, "r") as f:
            lines = f.readlines()
        with open(packages_path, "w") as f:
            for line in lines:
                # If line starts with "Filename: ./", replace that portion with the desired prefix
                if line.startswith("Filename: ./"):
                    line = line.replace("Filename: ./", f"Filename: {prefix}")
                f.write(line)
        print("[INFO] Updated Filename paths in Packages file.")

        packages_gz_path = os.path.join(apt_binary_dir, "Packages.gz")
        print("[INFO] Compressing Packages to Packages.gz...")
        with open(packages_gz_path, "wb") as f_out:
            subprocess.check_call(["gzip", "-9c", "Packages"], cwd=apt_binary_dir, stdout=f_out)
        print(f"[INFO] Created {packages_gz_path}")

        # 6) Generate a proper Release file with apt-ftparchive in debian/dists/stable
        os.makedirs(stable_dir, exist_ok=True)
        apt_ftparchive_conf_path = os.path.join(stable_dir, "apt-ftparchive.conf")
        conf_content = """APT::FTPArchive::Release {
Origin "ScaffoldsRepo";
Label "ScaffoldsRepo";
Suite "stable";
Codename "stable";
Architectures "amd64";
Components "main";
};
        """
        with open(apt_ftparchive_conf_path, "w") as f:
            f.write(conf_content)

        release_path = os.path.join(stable_dir, "Release")
        apt_ftparchive_cmd = ["apt-ftparchive", "-c", "apt-ftparchive.conf", "release", "."]
        print(f"[INFO] Running apt-ftparchive to generate Release (cwd={stable_dir})...")
        with open(release_path, "w") as release_file:
            subprocess.check_call(apt_ftparchive_cmd, cwd=stable_dir, stdout=release_file)
        print(f"[INFO] Generated Release at {release_path}")

        # 7) Optionally sign Release with GPG
        print("[INFO] Signing Release file with GPG...")
        sign_cmd = [
            "gpg",
            "--local-user", "172E2D67FB733C7EB47DEA047FE8FD47C68DC85A",
            "--detach-sign",
            "--armor",
            "--output", "Release.gpg",
            "Release"
        ]
        subprocess.check_call(sign_cmd, cwd=stable_dir)
        print("[INFO] Release file signed (Release.gpg created).")

        print("[INFO] prepare_deb_for_distribution completed successfully.")

    def push_to_server():
        """
        1) Parse ~/.rgwfuncs JSON to find the 'icdattcwsm' preset for ssh details.
        2) SSH into the server and remove the existing 'debian' dir under
           /home/rgw/Apps/frontend-sites/files.ryangerardwilson.com/scaffolds
        3) SCP (or rsync) the local 'debian' folder to that server path.
        """
        # Step 1) Read ~/.rgwfuncs and find icdattcwsm preset
        funcs_path = os.path.expanduser("~/.rgwfuncsrc")
        if not os.path.exists(funcs_path):
            raise FileNotFoundError(f"Cannot find config file: {funcs_path}")

        with open(funcs_path, "r") as f:
            data = json.load(f)

        vm_presets = data.get("vm_presets", [])
        preset = next((p for p in vm_presets if p.get("name") == "icdattcwsm"), None)
        if not preset:
            raise ValueError("No preset named 'icdattcwsm' found in ~/.rgwfuncs")

        host = preset["host"]
        ssh_user = preset["ssh_user"]
        ssh_key_path = preset["ssh_key_path"]

        # Step 2) SSH remove existing 'debian' on server
        remote_path = "/home/rgw/Apps/frontend-sites/files.ryangerardwilson.com/scaffolds"
        ssh_cmd = (
            f"ssh -i {ssh_key_path} {ssh_user}@{host} "
            f"'rm -rf {remote_path}/debian'"
        )
        print(f"[INFO] Removing existing debian directory on server: {remote_path}/debian")
        subprocess.check_call(ssh_cmd, shell=True)

        # Step 3) Rsync local 'debian' folder to server, excluding certain directories
        rsync_cmd = (
            f"rsync -avz -e 'ssh -i {ssh_key_path}' "
            f"--exclude 'version_build_folders' --exclude 'version_debs' "
            f"debian {ssh_user}@{host}:{remote_path}"
        )
        print(f"[INFO] Uploading local debian folder to {remote_path} on server (excluding version_build_folders and version_debs)")
        subprocess.check_call(rsync_cmd, shell=True)
        print("[INFO] push_to_server completed successfully.")

    def delete_all_but_last_version_build_folders():
        build_folders_path = "debian/version_build_folders"

        # List all the folders in the build_folders_path
        version_folders = [
            f for f in os.listdir(build_folders_path)
            if os.path.isdir(os.path.join(build_folders_path, f)) and f.startswith("scaffolds_")
        ]

        # Sort the folders based on version
        version_folders.sort(key=lambda x: parse_version(x.split('_')[1]), reverse=True)

        # Keep the last version only
        for folder in version_folders[1:]:
            full_path = os.path.join(build_folders_path, folder)
            print(f"[INFO] Deleting build folder: {full_path}")
            shutil.rmtree(full_path)

    def delete_all_but_last_version_debs():
        debs_dir = "debian/version_debs"

        # List all the .deb files
        deb_files = [
            f for f in os.listdir(debs_dir)
            if os.path.isfile(os.path.join(debs_dir, f)) and f.endswith(".deb")
        ]

        # Sort them based on version
        deb_files.sort(key=lambda x: parse_version(x.split('_')[1][:-4]), reverse=True)

        # Keep the last version only
        for deb in deb_files[1:]:
            full_path = os.path.join(debs_dir, deb)
            print(f"[INFO] Deleting deb file: {full_path}")
            os.remove(full_path)

    # version = get_new_version_number(MAJOR_RELEASE_NUMBER)
    build_deb(version)
    prepare_deb_for_distribution(version)
    push_to_server()
    delete_all_but_last_version_build_folders()
    delete_all_but_last_version_debs()


def step6_optional_publish_release(args, version):
    """
    Step VI: Check for the --publish_release flag, if present:
    """
    if "--publish" in args:
        print("[INFO] Step VI - Detected --publish flag.")
        publish_release(version)
        print("[INFO] Step VI Completed!")
    else:
        print("[INFO] Use the --publish flag to publish the latest release.")


################################################################################
# MAIN
################################################################################

def main():

    current_version, new_version = get_versions()

    # Step I: Preprocessing
    step1_preprocessing(sys.argv[1:], current_version, new_version)

    # Step II
    step2_compile()

    # Step V
    step5_optional_test_scaffold(sys.argv[1:])

    # Step VI
    step6_optional_publish_release(sys.argv[1:], new_version)
    print()


if __name__ == "__main__":
    main()
