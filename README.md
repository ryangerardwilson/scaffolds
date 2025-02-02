# Scaffolds

This tool is designed to perform scaffolding operations via the command line. Below you will find instructions on how to use this application effectively.

## Features

- Accepts a directory path as an argument and performs scaffolding operations on it.
- Prints beautiful ASCII art upon execution.

## Prerequisites

Before you can run this application, ensure you have the following installed:

- OCaml (and `ocamlc`, the OCaml compiler)
- Required OCaml Libraries (see below for installation instructions)

## Setup

1. **Clone the Repository:**

   git clone https://github.com/ryangerardwilson/scaffolds
   cd scaffolds

2. **Compile the Application:**

   # Navigate to where your `main.ml` file is located and run the OCaml compiler:
   python compile.py

## Usage

To run the application, execute it with the `--scaffold` flag followed by the directory path you want to scaffold:

    ./scaffolds --scaffold <target_directory>

Make sure to replace `<target_directory>` with the actual path where you want scaffolding operations to be executed.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

