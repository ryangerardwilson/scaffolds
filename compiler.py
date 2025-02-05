#!/usr/bin/env python3
import os
import sys
import subprocess
import shutil
import json
import re
from packaging.version import parse as parse_version


MAJOR_RELEASE_NUMBER = 2

###############################################################################
# STEP I: Preprocessing – Generate lib/templates_lib.ml and lib/scaffolder_lib.ml
###############################################################################

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

def replace_version_placeholder(file_path, version):
    with open(file_path, 'r') as file:
        content = file.read()

    # Replace the placeholder with the actual version
    new_content = re.sub(r'\{\{VERSION_PLACEHOLDER\}\}', version, content)

    # Optionally, write to a temporary file or overwrite
    with open(file_path, 'w') as file:
        file.write(new_content)


def build_variable_name(root_relative_path):
    """
    Given a path relative to 'src', build a variable name of the form:
    - If inside subdirs: dir_<dir1>_dir_<dir2>_..._file_<filename>_ext_<extension>
    - If in root with normal filename.ext: file_<filename>_ext_<extension>
    - If in root with pure extension (e.g., '.env'): ext_<extension_without_dot>
    """
    parts = root_relative_path.split(os.sep)

    # If there's only one part (i.e., file directly in src/):
    #   e.g., "main.ml" -> "file_main_ext_ml"
    #   e.g., ".env"    -> "ext_env"
    # If more subdirectories exist, we build out "dir_xxx" sections for each directory
    # before final "file_xxx_ext_xxx".

    # Remove any directories from the end that are actually the file name
    # so that 'parts[-1]' is the filename, and 'parts[:-1]' are directories.
    dirs = parts[:-1]
    filename = parts[-1]

    # Separate the filename from extension if it exists
    if filename.startswith('.'):  # pure extension, e.g. ".env"
        # var name like: ext_env
        var_name = f"ext_{filename[1:]}" if len(filename) > 1 else "ext_"  # handle edge cases
    else:
        # Split by last dot:
        if '.' in filename:
            name, ext = filename.rsplit('.', 1)
        else:
            # File with no dots => no extension
            name, ext = filename, ""

        if len(dirs) == 0:
            # in root of src:
            if ext == "":
                # no extension => "file_<filename>"
                var_name = f"file_{name}"
            else:
                var_name = f"file_{name}_ext_{ext}"
        else:
            # has directory parts
            dir_chunks = []
            for d in dirs:
                dir_chunks.append(f"dir_{d}")
            if ext == "":
                var_name = "_".join(dir_chunks + [f"file_{name}"])
            else:
                var_name = "_".join(dir_chunks + [f"file_{name}", f"ext_{ext}"])

    return var_name


def gather_files_from_src(src_dir):
    """
    Recursively gather all files in src_dir, ignoring 'app' (no extension) in the root.
    Returns a list of (relative_path, variable_name, file_content).
    relative_path is path relative to src_dir, used later for scaffolding.
    """
    collected = []
    for root, dirs, files in os.walk(src_dir):
        # For each file, figure out its relative path to src/
        for f in files:
            try:
                full_path = os.path.join(root, f)
                rel_path = os.path.relpath(full_path, src_dir)  # e.g., "lib/Home.ml"

                # Check if ignoring 'src/app' with no extension in root
                # That means if rel_path == "app" AND there's no '.' in "app"
                if rel_path == "app" and '.' not in rel_path:
                    continue

                # print("79", rel_path)
                # Check if ignoring 'src/.tailwindcss-linux-x64' in root
                if '.tailwindcss-linux-x64' in rel_path:
                    continue

                # Check if ignoring 'src/resources/assets/styles.css' in root
                if 'src/resources/assets/styles.css' in rel_path:
                    continue

                # Check if ignoring '.db'
                if '.db' in rel_path:
                    continue

                if '.swp' in rel_path:
                    continue

                # Build the variable name
                var_name = build_variable_name(rel_path)

                # Read the file content
                with open(full_path, 'r', encoding='utf-8') as infile:
                    content = infile.read()

                collected.append((rel_path, var_name, content))
            except Exception as e:
                full_path = os.path.join(root, f)
                print(e, full_path)
    return collected


def write_templates_lib(all_files):
    """
    Writes lib/templates_lib.ml with let-bindings like:
    let dir_lib_file_home_ext_ml = {| <content> |}
    ...
    """
    os.makedirs("lib", exist_ok=True)
    output_path = os.path.join("lib", "templates_lib.ml")
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("(* AUTO-GENERATED by compile.py Step 0 *)\n\n")
        for _, var_name, content in all_files:
            # Escape any {| or |} inside the content? Typically not needed if we trust the input,
            # but if your files had literal {| or |}, that could conflict with the syntax.
            f.write(f"let {var_name} = {{|\n{content}\n|}}\n\n")
        print(f"[INFO] Step I - Generated {output_path}")


def write_scaffolder_lib(all_files):
    """
    Write lib/scaffolder_lib.ml, including a scaffold function that:
      1) ensures directories exist
      2) writes each file from Templates_lib
      3) specifically writes compile.sh and chmod +x it
    """
    os.makedirs("lib", exist_ok=True)
    output_path = os.path.join("lib", "scaffolder_lib.ml")
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("(* AUTO-GENERATED by compile.py Step 0 *)\n\n")

        f.write("let ensure_dir path =\n")
        f.write("  if Sys.file_exists path then\n")
        f.write("    Printf.printf \"[INFO] scaffolds - Directory '%s' already exists, skipping creation.\\n\" path\n")
        f.write("  else begin\n")
        f.write("    Unix.mkdir path 0o755;\n")
        f.write("    Printf.printf \"[INFO] scaffolds - Created directory: %s\\n\" path\n")
        f.write("  end\n\n")

        f.write("let rec ensure_full_path path =\n")
        f.write("  if not (Sys.file_exists path) then begin\n")
        f.write("    ensure_full_path (Filename.dirname path);\n")
        f.write("    Unix.mkdir path 0o755;\n")
        f.write("    Printf.printf \"[INFO] scaffolds - Created directory: %s\\n\" path\n")
        f.write("  end\n\n")

        f.write("let write_file filename content =\n")
        f.write("  let oc = open_out filename in\n")
        f.write("  output_string oc content;\n")
        f.write("  close_out oc;\n")
        f.write("  Printf.printf \"[INFO] scaffolds - Created or updated file: %s\\n\" filename\n\n")

        f.write("let scaffold target_dir =\n")
        f.write("  ensure_full_path target_dir;\n\n")
        f.write("  let full_path sub_path = Filename.concat target_dir sub_path in\n\n")

        # For each file, we create directories and write files
        for rel_path, var_name, _ in all_files:
            sub_dir = os.path.dirname(rel_path)
            if sub_dir != "":
                f.write(f"  ensure_full_path (full_path \"{sub_dir}\");\n")
            f.write(f"  write_file (full_path \"{rel_path}\") Templates_lib.{var_name};\n\n")

        # After writing everything, specifically handle compile.sh
        f.write("  (* Make compiler executable *)\n")
        f.write("  let compiler_path = full_path \"compiler\" in\n")
        f.write("  if Sys.file_exists compiler_path then (\n")
        f.write("    Unix.chmod compiler_path 0o755;\n")
        f.write("    Printf.printf \"[INFO] scaffolds - Set +x on compiler\\n\"\n")
        f.write("  );\n\n")

        f.write("  print_endline \"[INFO] scaffolds - Scaffolding complete. You can now edit your files or compile.\";\n")

    print(f"[INFO] Step I - Generated {output_path}")


def step1_preprocessing(args, version):

    if "--publish" in args:
        print("[INFO] Step I - Preprocessing – Adding version number to main.ml")
        # Assume main.ml is in the same directory as this script
        script_dir = os.path.dirname(os.path.abspath(__file__))
        main_ml_path = os.path.join(script_dir, 'main.ml')

        # Replace the placeholder
        replace_version_placeholder(main_ml_path, version)

    print("[INFO] Step I - Preprocessing – Generating templates_lib.ml and scaffolder_lib.ml from src/ ...")
    src_dir = "src"
    if not os.path.isdir(src_dir):
        print(f"[WARNING] Step I - 'src' directory not found at {os.path.abspath(src_dir)}. Skipping generation.")
        return

    all_files = gather_files_from_src(src_dir)
    write_templates_lib(all_files)
    write_scaffolder_lib(all_files)
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
# ocamlc -c -I lib -I +unix lib/templates_lib.ml
# ocamlc -c -I lib -I +unix lib/scaffolder_lib.ml


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
    """
    print("[INFO] Step II - Compiling modules...")
    subprocess.run(["ocamlopt", "-c", "-I", "lib", "-I", "+unix", "lib/templates_lib.ml"], check=True)
    subprocess.run(["ocamlopt", "-c", "-I", "lib", "-I", "+unix", "lib/scaffolder_lib.ml"], check=True)


def step3_link():
    """
    Step III: Link modules with main (native code).
    """
    print("[INFO] Step III - Linking modules...")
    subprocess.run([
        "ocamlopt",
        "-I", "lib",
        "-I", "+unix",
        "-o", "scaffolds",
        "unix.cmxa",
        "lib/templates_lib.cmx",
        "lib/scaffolder_lib.cmx",
        "main.ml"
    ], check=True)


def step4_cleanup():
    """
    Step IV: Remove all compilation artifacts.
    """
    print("[INFO] Step IV - Cleaning up *.cmo, *.cmi, *.cmx, *.o, and *.out...")
    for root, dirs, files in os.walk("."):
        for f in files:
            if f.endswith((".cmo", ".cmi", ".cmx", ".o", ".out")):
                os.remove(os.path.join(root, f))


def step5_optional_test_scaffold(args):
    """
    Step V: Check for the --and_generate_test_scaffold flag, if present:
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

    version = get_new_version_number()

    # Step I: Preprocessing
    step1_preprocessing(sys.argv[1:], version)

    # Step II
    step2_compile()

    # Step III
    step3_link()
    print("[INFO] Step III - Compilation complete.")

    # Step IV
    step4_cleanup()

    # Step V
    step5_optional_test_scaffold(sys.argv[1:])

    # Step VI
    step6_optional_publish_release(sys.argv[1:], version)
    print()


if __name__ == "__main__":
    main()
