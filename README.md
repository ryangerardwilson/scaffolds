# Scaffolds

This tool is designed to perform scaffolding operations via the command line. Below you will find instructions on how to use this application effectively.

## 1. Features

- Accepts a directory path as an argument and performs scaffolding operations on it.

## 2. Prerequisites

Before you can run this application, ensure you have the following installed:
- OCaml (and `ocamlc`, the OCaml compiler)

## 3. Setup

### 3.1. Installation

    curl -fsSL https://files.ryangerardwilson.com/scaffolds/debian/pubkey.gpg | sudo gpg --dearmor -o /usr/share/keyrings/scaffolds.gpg; sudo echo "deb [arch=amd64 signed-by=/usr/share/keyrings/scaffolds.gpg] https://files.ryangerardwilson.com/scaffolds/debian stable main" | sudo tee /etc/apt/sources.list.d/scaffolds.list; sudo apt update; sudo apt-get install scaffolds

   curl -fsSL https://files.ryangerardwilson.com/scaffolds/debian/pubkey.gpg \
       | sudo gpg --dearmor -o /usr/share/keyrings/scaffolds.gpg
   echo "deb [arch=amd64 signed-by=/usr/share/keyrings/scaffolds.gpg] https://files.ryangerardwilson.com/scaffolds/debian stable main" \
       | sudo tee /etc/apt/sources.list.d/scaffolds.list
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

