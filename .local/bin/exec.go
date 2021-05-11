package main

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os/exec"
	"strconv"
	"strings"
)

type runData struct {
	stdin  io.WriteCloser
	stdout io.ReadCloser
	stderr io.ReadCloser
}

// ### use a type instead of two functions
func (this runData) readstringtrimmed() string {
	// ### error handling
	out, _ := ioutil.ReadAll(this.stdout)
	return strings.Trim(string(out), "\n")
}

func (this runData) readstring() string {
	// ### error handling
	out, _ := ioutil.ReadAll(this.stdout)
	return string(out)
}

type runContext struct {
	ignoreExitFailure bool
}

func (this runContext) run(proc string) runData {
	argv := strings.Split(proc, " ")
	return this.runSlice(argv[0], argv[1:]...)
}

func (this runContext) runSlice(name string, arg ...string) runData {
	cmd := exec.Command(name, arg...)
	stdoutp, err := cmd.StdoutPipe()
	if err != nil {
		panic(fmt.Sprintf("Error fetching stdout: %s", err))
	}
	stdinp, err := cmd.StdinPipe()
	if err != nil {
		panic(fmt.Sprintf("Error fetching stdin: %s", err))
	}
	stderrp, err := cmd.StderrPipe()
	if err != nil {
		panic(fmt.Sprintf("Error fetching stderr: %s", err))
	}
	if err := cmd.Start(); err != nil {
		panic(fmt.Sprintf("Error start: %s", err))
	}

	go func() {
		if err := cmd.Wait(); err != nil {
			if !this.ignoreExitFailure {
				stdouts, _ := ioutil.ReadAll(stdoutp)
				errout, _ := ioutil.ReadAll(stderrp)
				panic(fmt.Sprintf("Error wait: %s %s (%s) (%s)", name, err, stdouts, errout))
			} else {
				//log.Printf("%s exited OK", name)
			}
			//log.Printf("Error wait: %s %s", name, err)
		}
	}()

	return runData{
		stdin:  stdinp,
		stdout: stdoutp,
		stderr: stderrp,
	}
}

func pipe(a ...runData) runData {
	if len(a) < 2 {
		panic(fmt.Sprintf("Can't pipe with only one arg"))
	}

	// nothing to write at this point.
	a[0].stdin.Close()

	for i := 0; i < len(a)-1; i++ {
		left := a[i]
		right := a[i+1]
		go func() {
			io.Copy(right.stdin, left.stdout)
			right.stdin.Close()
		}()
	}

	return a[len(a)-1]
}
func splitlinebyte(r io.ReadCloser) chan []byte {
	ret := make(chan []byte)
	go func() {
		// ### error handling
		scanner := bufio.NewScanner(r)
		for scanner.Scan() {
			ret <- scanner.Bytes()
		}
		close(ret)
	}()
	return ret
}

func splitlinestring(r io.ReadCloser) chan string {
	ret := make(chan string)
	go func() {
		// ### error handling
		scanner := bufio.NewScanner(r)
		for scanner.Scan() {
			ret <- scanner.Text()
		}
		close(ret)
	}()
	return ret
}

func sp(format string, args ...interface{}) string {
	return fmt.Sprintf(format, args...)
}

// rough port of swaplist
func swapUsed() {
	rc := runContext{ignoreExitFailure: true} // permission denied errors
	run := func(str string) runData {
		return rc.run(str)
	}

	overall := 0
	r := pipe(run("find /proc/ -maxdepth 1 -type d"), run("egrep ^/proc/[0-9]"))
	for DIR := range splitlinestring(r.stdout) {
		PID := pipe(run(sp("echo %s", DIR)), run("cut -d / -f 3")).readstringtrimmed()
		PROGNAME := run(sp("ps -p %s -o comm --no-headers", PID)).readstringtrimmed()
		pswap := pipe(run(sp("grep Swap %s/smaps", DIR)), run("awk { print $2}"))

		sum := 0
		for SWAP := range splitlinestring(pswap.stdout) {
			stmp, _ := strconv.Atoi(SWAP)
			sum += stmp
		}
		overall += sum
		log.Printf("%s %d", PROGNAME, sum)
	}
	log.Printf("Total swap used: %d", overall)
}

func main() {
	swapUsed()
}
