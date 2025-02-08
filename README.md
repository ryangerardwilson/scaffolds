# Scaffolds

An OCaml-powered web application framework with syntax so pretty, you'd want to marry it!

## 1. Features

- Accepts a directory path as an argument and performs scaffolding operations on it.

## 2. Prerequisites

    # OS - This framework will always only be maintained for the most recent Ubuntu LTS
    Ubuntu 24.04.1 LTS

    # OCaml Version - This framework will always only be maintained for the most recent Ocaml Versions
    5.3.0

    # Opam
    sudo apt-get update; sudo apt-get install opam; opam init
    # Open a new terminal to re-initialize the shell
    eval $(opam env)

    # Ocamlfind & Other Dependencies
    opam install ocamlfind cohttp-lwt-unix dotenv sqlite3 yojson

## 3. Setup

### 3.1. Installation

    curl -fsSL https://files.ryangerardwilson.com/scaffolds/debian/pubkey.gpg | sudo gpg --dearmor -o /usr/share/keyrings/scaffolds.gpg
    echo "deb [arch=amd64 signed-by=/usr/share/keyrings/scaffolds.gpg] https://files.ryangerardwilson.com/scaffolds/debian stable main" | sudo tee /etc/apt/sources.list.d/scaffolds.list
    sudo apt update
    sudo apt-get install scaffolds

### 3.2. Subsequent Updates

    sudo apt update
    sudo apt upgrade

## 4. Usage

To run the application, execute it with the `--scaffold` flag followed by the directory path you want to scaffold:

    scaffolds --scaffold <target_directory>

Make sure to replace `<target_directory>` with the actual path where you want scaffolding operations to be executed.

## 5. License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.


