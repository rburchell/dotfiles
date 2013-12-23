/* Build, (with your favorite compiler) run:
 * cc -o vlcwrap vlcwrap.c
 *
 * thanks to: https://gist.github.com/Joshkunz/6410613
 */

#include <unistd.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>

#define VLC_BIN "/Applications/VLC.app/Contents/MacOS/VLC" 

pid_t child_pid = -1;

/* when we get an interrupt, kill the child process */
void sigint_handler(int signal) {
    kill(child_pid, SIGKILL);
}

int main (int argc, char* argv[]) {
    /* Create a new null-terminated array of arguments */
    char ** child_args = malloc(sizeof(char *) * (argc + 1));
    memcpy(child_args, argv, sizeof(char *) * argc);
    /* set the first argument to the program we're running */
    child_args[0] = VLC_BIN;
    child_args[argc] = NULL;

    signal(SIGINT, sigint_handler);

    child_pid = fork();

    /* if this is the child, run VLC */
    if (child_pid == 0) {
        execve(VLC_BIN, child_args, NULL);
    /* otherwise, wait for VLC to exit (or a signal to terminate it) */
    } else {
        waitpid(child_pid, NULL, 0);
    }

    return 0;
}


