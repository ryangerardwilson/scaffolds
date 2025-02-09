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

## 4. Quick Start

To run the application, execute it with the `--scaffold` flag followed by the directory path you want to scaffold:

    scaffolds --scaffold <target_directory>

Make sure to replace `<target_directory>` with the actual path where you want scaffolding operations to be executed.

Then, move into your project directory and source the Ocaml environment initialized by the compiler

    cd <target_directory>
    source ./compiler --init

Finally, run the app via systemctl, with the compiler. And, serve to nginx - assuming you have specified a `DOMAIN_NAME` in the .env, pointed its `A` record towards your IP address, and installed nginx and certbot.

    ./compiler --run-prod-server --nginx

See complete list of compiler flags below.

    source ./compiler --init                          set up the OCaml environment and source it.
    ./compiler --compile                              compile the app
    ./compiler --run-dev-server                       run the app in the current session
    ./compiler --run-prod-server                      run the app via systemctl
    ./compiler --stop-prod-server                     stop the instance running via systemctl
    ./compiler --restart-prod-server                  restart the instance running via systemctl
    ./compiler --delete-prod-service-file             delete the production systemd service file
    ./compiler --get-prod-server-status               get the production service status
    ./compiler --view-prod-server-logs                follow the production service logs
    ./compiler --nginx                                to configure nginx and sign ssl (requires DOMAIN_NAME in .env, and nginx+cerbot pre-installed)

## 5. License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.


