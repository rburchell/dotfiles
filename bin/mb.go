package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
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

type projectDetector interface {
	detect(filePath string) []project
}

type project interface {
	run()
}

type makefileDetector struct{}

func (this makefileDetector) detect(filePath string) []project {
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
		makefileDetector{},
	}

	for _, detector := range detectors {
		projects := detector.detect(os.Args[1])
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
