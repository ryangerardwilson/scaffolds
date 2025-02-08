package main

import (
	"embed"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strings"
)

var _ embed.FS

//go:embed src/main.ml
var file_main_ext_ml string

//go:embed src/.env
var ext_env string

//go:embed src/.gitignore
var ext_gitignore string

//go:embed src/.tailwind_build_input
var ext_tailwind_build_input string

//go:embed src/compiler
var file_compiler string

//go:embed src/dbs/logs/schema.sql
var dir_dbs_dir_logs_file_schema_ext_sql string

//go:embed src/dbs/auth/schema.sql
var dir_dbs_dir_auth_file_schema_ext_sql string

//go:embed src/utils/migrations.ml
var dir_utils_file_migrations_ext_ml string

//go:embed src/utils/debugger.ml
var dir_utils_file_debugger_ext_ml string

//go:embed src/utils/authentication.ml
var dir_utils_file_authentication_ext_ml string

//go:embed src/utils/database.ml
var dir_utils_file_database_ext_ml string

//go:embed src/utils/renderer.ml
var dir_utils_file_renderer_ext_ml string

//go:embed src/resources/signup.html
var dir_resources_file_signup_ext_html string

//go:embed src/resources/debugger.html
var dir_resources_file_debugger_ext_html string

//go:embed src/resources/about.html
var dir_resources_file_about_ext_html string

//go:embed src/resources/login.html
var dir_resources_file_login_ext_html string

//go:embed src/resources/landing.html
var dir_resources_file_landing_ext_html string

//go:embed src/resources/dashboard.html
var dir_resources_file_dashboard_ext_html string

//go:embed src/resources/assets/styles.css
var dir_resources_dir_assets_file_styles_ext_css string

//go:embed src/lib/login.ml
var dir_lib_file_login_ext_ml string

//go:embed src/lib/logout.ml
var dir_lib_file_logout_ext_ml string

//go:embed src/lib/signup.ml
var dir_lib_file_signup_ext_ml string

//go:embed src/lib/about.ml
var dir_lib_file_about_ext_ml string

//go:embed src/lib/assets.ml
var dir_lib_file_assets_ext_ml string

//go:embed src/lib/dashboard.ml
var dir_lib_file_dashboard_ext_ml string

//go:embed src/lib/landing.ml
var dir_lib_file_landing_ext_ml string

func ensureFullPath(path string) error {
	return os.MkdirAll(path, 0755)
}

func writeFile(filename, content string) error {
	err := ioutil.WriteFile(filename, []byte(content), 0644)
	if err != nil {
		return err
	}
	fmt.Printf("[INFO] scaffolds - Created or updated file: %s\n", filename)
	return nil
}

func Scaffold(targetDir string) error {
	if err := ensureFullPath(targetDir); err != nil {
		return err
	}
	fullPath := func(subPath string) string { return filepath.Join(targetDir, subPath) }

	if err := writeFile(fullPath("main.ml"), file_main_ext_ml); err != nil {
		return err
	}

	if err := writeFile(fullPath(".env"), ext_env); err != nil {
		return err
	}

	if err := writeFile(fullPath(".gitignore"), ext_gitignore); err != nil {
		return err
	}

	if err := writeFile(fullPath(".tailwind_build_input"), ext_tailwind_build_input); err != nil {
		return err
	}

	if err := writeFile(fullPath("compiler"), file_compiler); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("dbs/logs")); err != nil {
		return err
	}
	if err := writeFile(fullPath("dbs/logs/schema.sql"), dir_dbs_dir_logs_file_schema_ext_sql); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("dbs/auth")); err != nil {
		return err
	}
	if err := writeFile(fullPath("dbs/auth/schema.sql"), dir_dbs_dir_auth_file_schema_ext_sql); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("utils")); err != nil {
		return err
	}
	if err := writeFile(fullPath("utils/migrations.ml"), dir_utils_file_migrations_ext_ml); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("utils")); err != nil {
		return err
	}
	if err := writeFile(fullPath("utils/debugger.ml"), dir_utils_file_debugger_ext_ml); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("utils")); err != nil {
		return err
	}
	if err := writeFile(fullPath("utils/authentication.ml"), dir_utils_file_authentication_ext_ml); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("utils")); err != nil {
		return err
	}
	if err := writeFile(fullPath("utils/database.ml"), dir_utils_file_database_ext_ml); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("utils")); err != nil {
		return err
	}
	if err := writeFile(fullPath("utils/renderer.ml"), dir_utils_file_renderer_ext_ml); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("resources")); err != nil {
		return err
	}
	if err := writeFile(fullPath("resources/signup.html"), dir_resources_file_signup_ext_html); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("resources")); err != nil {
		return err
	}
	if err := writeFile(fullPath("resources/debugger.html"), dir_resources_file_debugger_ext_html); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("resources")); err != nil {
		return err
	}
	if err := writeFile(fullPath("resources/about.html"), dir_resources_file_about_ext_html); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("resources")); err != nil {
		return err
	}
	if err := writeFile(fullPath("resources/login.html"), dir_resources_file_login_ext_html); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("resources")); err != nil {
		return err
	}
	if err := writeFile(fullPath("resources/landing.html"), dir_resources_file_landing_ext_html); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("resources")); err != nil {
		return err
	}
	if err := writeFile(fullPath("resources/dashboard.html"), dir_resources_file_dashboard_ext_html); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("resources/assets")); err != nil {
		return err
	}
	if err := writeFile(fullPath("resources/assets/styles.css"), dir_resources_dir_assets_file_styles_ext_css); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("lib")); err != nil {
		return err
	}
	if err := writeFile(fullPath("lib/login.ml"), dir_lib_file_login_ext_ml); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("lib")); err != nil {
		return err
	}
	if err := writeFile(fullPath("lib/logout.ml"), dir_lib_file_logout_ext_ml); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("lib")); err != nil {
		return err
	}
	if err := writeFile(fullPath("lib/signup.ml"), dir_lib_file_signup_ext_ml); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("lib")); err != nil {
		return err
	}
	if err := writeFile(fullPath("lib/about.ml"), dir_lib_file_about_ext_ml); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("lib")); err != nil {
		return err
	}
	if err := writeFile(fullPath("lib/assets.ml"), dir_lib_file_assets_ext_ml); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("lib")); err != nil {
		return err
	}
	if err := writeFile(fullPath("lib/dashboard.ml"), dir_lib_file_dashboard_ext_ml); err != nil {
		return err
	}

	if err := ensureFullPath(fullPath("lib")); err != nil {
		return err
	}
	if err := writeFile(fullPath("lib/landing.ml"), dir_lib_file_landing_ext_ml); err != nil {
		return err
	}

	compilerPath := fullPath("compiler")
	if _, err := os.Stat(compilerPath); err == nil {
		if err := os.Chmod(compilerPath, 0755); err != nil {
			log.Printf("[ERROR] scaffolds - Could not set executable permission on compiler: %v\n", err)
		} else {
			fmt.Println("[INFO] scaffolds - Set +x on compiler")
		}
	}

	fmt.Println("[INFO] scaffolds - Scaffolding complete. You can now edit your files or compile.")
	return nil
}

func getScaffoldDirectory() (string, error) {
	args := os.Args[1:]
	for i, arg := range args {
		if arg == "--new" {
			if i+1 < len(args) {
				target := args[i+1]
				if strings.HasPrefix(target, "--") {
					return "", fmt.Errorf("no valid directory specified after '--new'")
				}
				return target, nil
			}
			return "", fmt.Errorf("no argument found after '--new'")
		}
	}
	return "", fmt.Errorf("'--new' flag not found")
}

func main() {
	// Print ASCII art banner
	const asciiArt = `

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
    
`
	fmt.Println(asciiArt)

	version := "2.0.26-1"
	fmt.Printf("Version: %s\n", version)

	targetPath, err := getScaffoldDirectory()
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		fmt.Println("Program requires the name of the new directory where it will scaffold your project after the '--new' flag.")
		os.Exit(1)
	}

	if err := Scaffold(targetPath); err != nil {
		fmt.Printf("Scaffolding failed: %v\n", err)
		os.Exit(1)
	}
}
