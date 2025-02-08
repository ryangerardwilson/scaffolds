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


def replace_version(main_hs_path, package_yaml_path, new_version):
    """
    Replaces version strings in both the Haskell source file and package.yaml file with new_version.

    For main_hs_path, it assumes a version line like:
         let version = "2.0.26-1"
    For package_yaml_path, it assumes a version line like:
         version: "0.1.0.0"
    and replaces it so that the new version is always quoted.

    In particular, if new_version is "2.0.27-1", then main.hs gets "2.0.27-1"
    while package.yaml gets "2.0.27.1".
    """

    # --- Update the Haskell source file ---
    with open(main_hs_path, 'r', encoding='utf-8') as f:
        main_content = f.read()

    # Regular expression for Haskell version definition:
    hs_pattern = r'(let\s+version\s*=\s*")[^"]*(")'

    def hs_replacer(match):
        return match.group(1) + new_version + match.group(2)
    new_main_content = re.sub(hs_pattern, hs_replacer, main_content)

    with open(main_hs_path, 'w', encoding='utf-8') as f:
        f.write(new_main_content)
    print(f"[INFO] Replaced version with {new_version} in {main_hs_path}")

    # --- Convert new_version for package.yaml ---
    # Replace hyphens with dots for package.yaml
    pkg_new_version = new_version.replace("-", ".")

    # --- Update the package.yaml file ---
    with open(package_yaml_path, 'r', encoding='utf-8') as f:
        pkg_content = f.read()

    # Regular expression for YAML version field at the beginning of a line.
    # It will match versions with or without quotes.
    pkg_pattern = r'^(version:\s*)["\']?([^"\n]+)["\']?'

    def pkg_replacer(match):
        # Always force double quotes around the new version.
        return match.group(1) + '"' + pkg_new_version + '"'

    new_pkg_content = re.sub(pkg_pattern, pkg_replacer, pkg_content, flags=re.MULTILINE)

    with open(package_yaml_path, 'w', encoding='utf-8') as f:
        f.write(new_pkg_content)
    print(f"[INFO] Replaced version with {pkg_new_version} in {package_yaml_path}")


def gather_files_from_src(src_dir):
    """
    Recursively gather files in src_dir.
    Returns a list of tuples: (relative_path, variable_name, file_content).
    """

    def build_variable_name(root_relative_path):
        """
        Given a path relative to your resource base, build a Haskell-style variable name.
        Example rules:
          - "main.ml" → "fileMainExtMl"
          - ".env" → "extEnv"
          - "dbs/logs/schema.sql" → "dbsLogsSchema"
        """

        def to_camel(s, capitalize_first=True):
            """Convert a string to CamelCase."""
            parts = re.split(r'[\s_-]+', s)
            if not parts:
                return ""
            if capitalize_first:
                return ''.join(word.capitalize() for word in parts)
            else:
                return parts[0].lower() + ''.join(word.capitalize() for word in parts[1:])

        parts = root_relative_path.split(os.sep)
        if len(parts) == 1:
            filename = parts[0]
            if filename.startswith('.'):
                base = filename[1:]
                return "ext" + to_camel(base)
            else:
                if '.' in filename:
                    name, ext = filename.rsplit('.', 1)
                    return "file" + to_camel(name) + "Ext" + to_camel(ext)
                else:
                    return "file" + to_camel(filename)
        else:
            dirs = parts[:-1]
            filename = parts[-1]
            first_dir = dirs[0].lower()
            remaining_dirs = "".join(to_camel(d, capitalize_first=True) for d in dirs[1:])
            dir_part = first_dir + remaining_dirs
            if filename.startswith('.'):
                base = filename[1:]
                return "ext" + to_camel(base)
            else:
                if '.' in filename:
                    name, _ = filename.rsplit('.', 1)
                    return dir_part + to_camel(name)
                else:
                    return dir_part + to_camel(filename)

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


def generate_templates_hs_module(file_paths, output_path):
    """
    Given a list of file paths (relative to some base such as "src_ocaml_project/"), generate a Haskell module
    called Templates that declares variables for each file using file embedding.

    file_paths: a list of tuples. Each tuple can have either:
       (path, comment) or (path, variable_name, file_content)
    This function will use the first element (the file path) and the second element as a comment.

    output_path: the file system path where the Templates.hs file should be written.
    """

    def to_camel(s, capitalize_first=True):
        """Convert a string to CamelCase."""
        parts = re.split(r'[\s_-]+', s)
        if not parts:
            return ""
        if capitalize_first:
            return ''.join(word.capitalize() for word in parts)
        else:
            return parts[0].lower() + ''.join(word.capitalize() for word in parts[1:])

    def build_variable_name(root_relative_path):
        """
        Given a path relative to your resource base, build a Haskell variable name.
        Example rules:
          - "main.ml" → "fileMainExtMl"
          - ".env" → "extEnv"
          - "dbs/logs/schema.sql" → "dbsLogsSchema"
        """
        parts = root_relative_path.split(os.sep)
        if len(parts) == 1:
            filename = parts[0]
            if filename.startswith('.'):
                base = filename[1:]
                return "ext" + to_camel(base)
            else:
                if '.' in filename:
                    name, ext = filename.rsplit('.', 1)
                    return "file" + to_camel(name) + "Ext" + to_camel(ext)
                else:
                    return "file" + to_camel(filename)
        else:
            dirs = parts[:-1]
            filename = parts[-1]
            first_dir = dirs[0].lower()
            remaining_dirs = "".join(to_camel(d, capitalize_first=True) for d in dirs[1:])
            dir_part = first_dir + remaining_dirs
            if filename.startswith('.'):
                base = filename[1:]
                return "ext" + to_camel(base)
            else:
                if '.' in filename:
                    name, ext = filename.rsplit('.', 1)
                    # For subdirectory files, we omit the extension as in your examples.
                    return dir_part + to_camel(name)
                else:
                    return dir_part + to_camel(filename)

    header = (
        "{-# LANGUAGE TemplateHaskell #-}\n"
        "{-# LANGUAGE CPP #-}\n"
        "module Templates\n"
        "  ( "  # we could list exports here
    )
    # Collect all variable names for the export list:
    var_names = []
    embed_lines = []
    for entry in file_paths:
        # Depending on tuple length, unpack appropriately.
        if len(entry) == 2:
            file_path, comment = entry
        elif len(entry) >= 3:
            file_path, comment, _ = entry   # ignore the third element
        else:
            raise ValueError("Each tuple in file_paths must have at least 2 elements.")
        var_name = build_variable_name(file_path)
        var_names.append(var_name)
        # Create an embedding line:
        embed_line = f'{var_name} :: ByteString\n'
        embed_line += f'{var_name} = $(embedFile "src_ocaml_project/{file_path}")'
        if comment:
            embed_line += f'  -- {comment}'
        embed_lines.append(embed_line)

    export_list = ", ".join(var_names)
    header += export_list + "\n  ) where\n\n"

    imports = (
        "import Data.ByteString (ByteString)\n"
        "import Data.FileEmbed (embedFile)\n\n"
    )

    body = "\n\n".join(embed_lines)

    module_text = header + imports + body + "\n"

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(module_text)

    print(f"[INFO] STEP I - Generated {output_path}")


def generate_scaffolder_hs_module(gathered_files, output_path):
    """
    Generates the Haskell module for Scaffolder, dynamically building a
    list of templates from the gathered files.

    gathered_files: A list of tuples (rel_path, variable_name, file_content).
      The file_content is not embedded here (we assume that the variables are
      defined in the Templates module). We simply use the variable_name as a reference.

    output_path: The path where the Scaffolder.hs file will be written (e.g. "lib/Scaffolder.hs" or "src/Scaffolder.hs").
    """
    header = """{-# LANGUAGE CPP #-}
module Scaffolder
  ( scaffold
  , writeFileWithInfo
  , ensureDir
  ) where

import qualified Data.ByteString as BS
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.FilePath ((</>), takeDirectory)
import Control.Monad (when)

#ifndef mingw32_HOST_OS
import System.Posix.Files ( getFileStatus, setFileMode, fileMode
                          , ownerExecuteMode, groupExecuteMode, otherExecuteMode
                          )
import Data.Bits ((.|.))
#endif

import Templates

"""
    # Build the templates list using each tuple’s relative path and variable name.
    templates_lines = []
    for rel_path, var_name, content in gathered_files:
        # Use proper Haskell string literal syntax for the file path.
        templates_lines.append(f'  ("{rel_path}", {var_name})')
    templates_list = "templates :: [(FilePath, BS.ByteString)]\ntemplates =\n  [\n" + ",\n".join(templates_lines) + "\n  ]\n\n"

    main_function = """-- | Write a file and print an informational message.
writeFileWithInfo :: FilePath -> BS.ByteString -> IO ()
writeFileWithInfo path content = do
  BS.writeFile path content
  putStrLn $ "[INFO] scaffolds - Created or updated file: " ++ path

-- | Ensure that a directory exists.
ensureDir :: FilePath -> IO ()
ensureDir = createDirectoryIfMissing True

-- | The main scaffolding function: creates directories and writes out embedded files.
scaffold :: FilePath -> IO ()
scaffold targetDir = do
  ensureDir targetDir
  let fullPath sub = targetDir </> sub
  mapM_ (\\(rel, fileData) -> do
          let dir = takeDirectory rel
          if not (null dir)
            then ensureDir (fullPath dir)
            else return ()
          writeFileWithInfo (fullPath rel) fileData
        ) templates

  -- Set executable permission on the "compiler" file.
  let compilerPath = fullPath "compiler"
  exists <- doesFileExist compilerPath
  when exists $ do
#ifndef mingw32_HOST_OS
    status <- getFileStatus compilerPath
    let newMode = fileMode status .|. ownerExecuteMode .|. groupExecuteMode .|. otherExecuteMode
    setFileMode compilerPath newMode
    putStrLn "[INFO] scaffolds - Set +x on compiler"
#else
    putStrLn "[INFO] scaffolds - Skipping setting executable permission (Windows)"
#endif
  putStrLn "[INFO] scaffolds - Scaffolding complete. You can now edit your files or compile."
"""
    module_text = header + templates_list + main_function
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(module_text)
    print(f"[INFO] STEP I - Generated {output_path}")


def step1_preprocessing(args, current_version, new_version):

    print("[INFO] Step I - Preprocessing – Generating main.go")
    script_dir = os.path.dirname(os.path.abspath(__file__))
    src_dir = "src_ocaml_project"
    if not os.path.isdir(src_dir):
        print(f"[ERROR] STEP I - Ocaml directory not found at {os.path.abspath(src_dir)}")
        sys.exit(1)
    all_files = gather_files_from_src(src_dir)
    if not all_files:
        print("[WARNING] STEP I - No files found in src/.")
    templates_hs_path = os.path.join(script_dir, 'lib/Templates.hs')
    scaffolder_hs_path = os.path.join(script_dir, 'lib/Scaffolder.hs')
    generate_templates_hs_module(all_files, templates_hs_path)
    generate_scaffolder_hs_module(all_files, scaffolder_hs_path)

    if "--publish" in args:
        print("[INFO] Step I - Preprocessing – Adding version number to main.ml")
        # Assume main.ml is in the same directory as this script
        main_hs_path = os.path.join(script_dir, 'app/Main.hs')
        package_yaml_path = os.path.join(script_dir, 'package.yaml')

        # Replace the placeholder
        replace_version(main_hs_path, package_yaml_path, new_version)

    print("[INFO] Step I - Preprocessing complete")

################################################################################
# STEP II: Compile
################################################################################


def step2_compile():
    """
    Step II: Build and install the Haskell project using Stack.
    """
    print("[INFO] STEP II - Now building the Haskell project using Stack...")
    env = os.environ.copy()

    # Build the project
    build_cmd = ["stack", "build"]
    subprocess.run(build_cmd, check=True, env=env)

    print("[INFO] STEP II - Build complete.")

    # By setting the verbosity level to error, we suppress warnings while installing.
    install_cmd = ["stack", "install", "--verbosity=error"]
    subprocess.run(install_cmd, check=True, env=env)

    print("[INFO] STEP II - Binary generated.")

##############################################################################
# STEP III - Test or Publish
##############################################################################


def step3_optional_test_scaffold(args):
    """
    Step V: Check for the --test flag, if present:
        - remove 'test' dir if exists
        - run ./scaffolds --new test_project
    """
    if "--test" in args:
        print("[INFO] Step III - Detected --and_generate_test_scaffold flag.")
        if os.path.isdir("test"):
            print("[INFO] Step III - Removing existing 'test' directory...")
            subprocess.run(["rm", "-rf", "test_ocaml_project"], check=True)
        print("[INFO] Step III - Generating test scaffold via ./scaffolds --new test_ocaml_project")
        subprocess.run(["./scaffolds", "--new", "test_ocaml_project"], check=True)
        print("[INFO] Step III Completed!")
    else:
        print("[INFO] Use the --test flag to generate the test scaffold.")


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
        print(f"[INFO] STEP III - Running: {' '.join(cmd)}")
        subprocess.check_call(cmd)
        print(f"[INFO] STEP III - Finished building {output_deb}")

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
            print("[INFO] STEP III - Removing previous debian/dists/stable folder to ensure a clean slate.")
            shutil.rmtree(stable_dir)

        # 2) Re-create apt_binary_dir
        apt_binary_dir = "debian/dists/stable/main/binary-amd64"
        os.makedirs(apt_binary_dir, exist_ok=True)

        # 3) Ensure an overrides.txt file is present
        overrides_path = os.path.join(apt_binary_dir, "overrides.txt")
        if not os.path.exists(overrides_path):
            with open(overrides_path, "w") as f:
                f.write("scaffolds optional utils\n")
        print(f"[INFO] STEP III - overrides.txt verified at {overrides_path}")

        # 4) Copy the newly built .deb into the apt_binary_dir
        deb_source = os.path.join("debian/version_debs", f"scaffolds_{version}.deb")
        print(f"[INFO] STEP III - Copying {deb_source} to {apt_binary_dir}")
        shutil.copy2(deb_source, apt_binary_dir)

        # 5) Generate Packages and Packages.gz with dpkg-scanpackages
        pkg_cmd = [
            "dpkg-scanpackages",
            "--multiversion",
            ".",
            "overrides.txt"
        ]
        packages_path = os.path.join(apt_binary_dir, "Packages")
        print("[INFO] STEP III - Generating Packages via dpkg-scanpackages...")
        with open(packages_path, "w") as pf:
            subprocess.check_call(pkg_cmd, cwd=apt_binary_dir, stdout=pf)
        print(f"[INFO] STEP III - Created {packages_path}")

        prefix = "dists/stable/main/binary-amd64/"
        print("[INFO] STEP III - Adjusting 'Filename:' entries to remove './'...")
        with open(packages_path, "r") as f:
            lines = f.readlines()
        with open(packages_path, "w") as f:
            for line in lines:
                # If line starts with "Filename: ./", replace that portion with the desired prefix
                if line.startswith("Filename: ./"):
                    line = line.replace("Filename: ./", f"Filename: {prefix}")
                f.write(line)
        print("[INFO] STEP III - Updated Filename paths in Packages file.")

        packages_gz_path = os.path.join(apt_binary_dir, "Packages.gz")
        print("[INFO] STEP III - Compressing Packages to Packages.gz...")
        with open(packages_gz_path, "wb") as f_out:
            subprocess.check_call(["gzip", "-9c", "Packages"], cwd=apt_binary_dir, stdout=f_out)
        print(f"[INFO] STEP III - Created {packages_gz_path}")

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
        print(f"[INFO] STEP III - Running apt-ftparchive to generate Release (cwd={stable_dir})...")
        with open(release_path, "w") as release_file:
            subprocess.check_call(apt_ftparchive_cmd, cwd=stable_dir, stdout=release_file)
        print(f"[INFO] STEP III - Generated Release at {release_path}")

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
        print("[INFO] STEP III - Release file signed (Release.gpg created).")

        print("[INFO] STEP III - prepare_deb_for_distribution completed successfully.")

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
        print(f"[INFO] STEP III - Removing existing debian directory on server: {remote_path}/debian")
        subprocess.check_call(ssh_cmd, shell=True)

        # Step 3) Rsync local 'debian' folder to server, excluding certain directories
        rsync_cmd = (
            f"rsync -avz -e 'ssh -i {ssh_key_path}' "
            f"--exclude 'version_build_folders' --exclude 'version_debs' "
            f"debian {ssh_user}@{host}:{remote_path}"
        )
        print(f"[INFO] STEP III - Uploading local debian folder to {remote_path} on server (excluding version_build_folders and version_debs)")
        subprocess.check_call(rsync_cmd, shell=True)
        print("[INFO] STEP III - push_to_server completed successfully.")

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
            print(f"[INFO] STEP III - Deleting build folder: {full_path}")
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
            print(f"[INFO] STEP III - Deleting deb file: {full_path}")
            os.remove(full_path)

    # version = get_new_version_number(MAJOR_RELEASE_NUMBER)
    build_deb(version)
    prepare_deb_for_distribution(version)
    push_to_server()
    delete_all_but_last_version_build_folders()
    delete_all_but_last_version_debs()


def step3_optional_publish_release(args, version):
    """
    Step VI: Check for the --publish_release flag, if present:
    """
    if "--publish" in args:
        print("[INFO] Step III - Detected --publish flag.")
        publish_release(version)
        print("[INFO] Step III Completed!")
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

    # Step III
    step3_optional_test_scaffold(sys.argv[1:])
    step3_optional_publish_release(sys.argv[1:], new_version)
    print()


if __name__ == "__main__":
    main()
