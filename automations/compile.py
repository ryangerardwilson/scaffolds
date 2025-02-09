#!/usr/bin/env python3
import os
import sys
import re


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


def main():

    print("[INFO] Step I - Preprocessing – Generating main.go")
    script_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.abspath(os.path.join(script_dir, os.pardir))
    src_dir = "src_ocaml_project"
    if not os.path.isdir(src_dir):
        print(f"[ERROR] STEP I - Ocaml directory not found at {os.path.abspath(src_dir)}")
        sys.exit(1)
    all_files = gather_files_from_src(src_dir)
    if not all_files:
        print("[WARNING] STEP I - No files found in src/.")
    templates_hs_path = os.path.join(parent_dir, 'lib/Templates.hs')
    scaffolder_hs_path = os.path.join(parent_dir, 'lib/Scaffolder.hs')
    generate_templates_hs_module(all_files, templates_hs_path)
    generate_scaffolder_hs_module(all_files, scaffolder_hs_path)

    print("[INFO] Step I - Preprocessing complete")


if __name__ == "__main__":
    main()
