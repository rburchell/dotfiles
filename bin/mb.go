package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path"
	"strings"
)

type cbType func(part, soFar string) (*string, error)

// For each part in "path", invoke callback with the current part,
// and the full path that has been walked already.
func forEachDirectoryPart(path string, callback cbType) (*string, error) {
	parts := strings.Split(path, "/")
	builtPath := ""

	for _, part := range parts {
		if part == "" {
			continue
		}

		ret, err := callback(part, builtPath)
		if err != nil {
			return nil, err
		}
		if ret != nil {
			return ret, nil
		}

		if part != "/" {
			builtPath += "/" + part
		}
	}
	return nil, nil
}

// Detect projects from a path
type projectDetector interface {
	// Called with the pathname to build from.
	// Return any projects that can be built here, or nil.
	detect(filePath string) []project
}

// A project is a thing that can be built:
// This might mean "make", or whatever.
type project interface {
	run()
}

// A simple detector for Makefile-based projects.
type genericMakefileDetector struct{}

func (this genericMakefileDetector) detect(filePath string) []project {
	var cb cbType = func(dirPart, builtPath string) (*string, error) {
		if _, err := os.Stat(fmt.Sprintf("%s/Makefile", builtPath)); err == nil {
			ret := builtPath
			return &ret, nil
		}
		return nil, nil
	}
	mfPath, err := forEachDirectoryPart(filePath, cb)
	if err != nil {
		panic(err)
	}
	if mfPath != nil {
		log.Printf("Makefile found in %s", *mfPath)
		p := makeProject{dir: *mfPath}
		return []project{p}
	}
	return nil
}

// Greenfield project
type greenfieldDetector struct{}

func (this greenfieldDetector) detect(filePath string) []project {
	var cb cbType = func(dirPart, builtPath string) (*string, error) {
		if dirPart == "greenfield" {
			makeCmd := fmt.Sprintf("source ~/.ssh/hosts/adele.home.viroteck.net.sh && cd %s/greenfield && cmake . -DCMAKE_BUILD_TYPE=Debug && make -j10 && ./AppGreenfield", builtPath)
			return &makeCmd, nil
		}
		return nil, nil
	}
	cmdString, err := forEachDirectoryPart(filePath, cb)
	if err != nil {
		panic(err)
	}
	if cmdString != nil {
		log.Printf("Greenfield found in %s", *cmdString)
		p := execBashProject{cmd: *cmdString}
		return []project{p}
	}
	return nil
}

// Project that is built by some generic shell-based command
// makeProject could be replaced by this...
type execBashProject struct {
	cmd string
}

func (this execBashProject) run() {
	cmd := exec.Command("/usr/bin/bash", "-c", this.cmd)
	//cmd.Dir = this.dir
	cmd.Stderr = os.Stderr
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout

	log.Printf("Starting bash-based command: %s", this.cmd)
	err := cmd.Start()
	if err != nil {
		log.Fatalf("Start error: %s", err)
	}
	log.Printf("Waiting for compile to finish...")
	err = cmd.Wait()
	if err == nil {
		os.Exit(0)
	} else {
		log.Fatalf("compile error: %v", err)
	}
}

// A simple detector for Go-based projects.
type genericGoDetector struct{}

func (this genericGoDetector) detect(filePath string) []project {
	log.Printf("Detecting Go on %s", filePath)
	if strings.HasSuffix(filePath, ".go") {
		return []project{
			goProject{
				dir: path.Dir(filePath),
			},
		}
	}
	return nil
}

// Go-based projects.
type goProject struct {
	dir string
}

func (this goProject) run() {
	cmd := exec.Command("/usr/bin/go", "build")
	cmd.Dir = this.dir
	cmd.Stderr = os.Stderr
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout

	log.Printf("Starting Go compile in %s...", this.dir)
	err := cmd.Start()
	if err != nil {
		log.Fatalf("Start error: %s", err)
	}
	log.Printf("Waiting for compile to finish...")
	err = cmd.Wait()
	if err != nil {
		log.Fatalf("compile error: %v", err)
	}

	cmd = exec.Command("/usr/bin/go", "test", "-v", "./...")
	cmd.Dir = this.dir
	cmd.Stderr = os.Stderr
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout

	log.Printf("Starting Go test in %s...", this.dir)
	err = cmd.Start()
	if err != nil {
		log.Fatalf("Start error: %s", err)
	}
	log.Printf("Waiting for tests to finish...")
	err = cmd.Wait()
	if err == nil {
		os.Exit(0)
	} else {
		log.Fatalf("tests error: %v", err)
	}
}

// A simple runner for "make"
type makeProject struct {
	dir string
}

func (this makeProject) run() {
	cmd := exec.Command("/usr/bin/make")
	cmd.Dir = this.dir
	cmd.Stderr = os.Stderr
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout

	log.Printf("Starting make in %s...", this.dir)
	err := cmd.Start()
	if err != nil {
		log.Fatalf("Start error: %s", err)
	}
	log.Printf("Waiting for make to finish...")
	err = cmd.Wait()
	if err == nil {
		os.Exit(0)
	} else {
		log.Fatalf("make error: %v", err)
	}
}

func main() {
	detectors := []projectDetector{
		greenfieldDetector{},

		// Generic detectors should be last.
		genericMakefileDetector{},
		genericGoDetector{},
	}

	// Work with -- in the arg list.
	flag.Parse()
	fa := flag.NArg()
	filePath := flag.Arg(fa - 1)

	for _, detector := range detectors {
		projects := detector.detect(filePath)
		if len(projects) == 1 {
			projects[0].run()
			panic("Project did not run?")
		} else if len(projects) > 1 {
			log.Printf("Projects: %+v", projects)
			log.Fatalf("Project detection was ambiguous; what to do?")
		}
	}

	log.Fatalf("No projects detected")
}
