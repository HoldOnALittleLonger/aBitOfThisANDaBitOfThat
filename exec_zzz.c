/**
 * @exec_zzz is a simple program just fork and exec the shell
 * script @zzz which is located at /usr/bin/zzz,it is a native
 * sleep script comes with Void Linux.
 * The automatically sleeping featured by KDE6 sometimes works
 * weird,but the computer can enter sleep state if I execute the
 * script manually.
 * We need to setuid to _zero_ to prevent the possible exception
 * that would be thrown by /usr/bin/nvidia-sleep.sh .
 * So this program is response to NVIDIA Display Card installed
 * machine.
 * # We do not use @vlogger to record logs in this program,
 *   rather to make a shell script to do that.
 * # Need setuid by root -- chmod u+s exec_zzz
 * # This program does not verify @zzz script,user must ensure
 *   the script is not a malware!
 */

#include <unistd.h>

#include <stddef.h>
#include <stdio.h>
#include <errno.h>

const char *zzz_path = "/usr/bin/zzz";

static int child_process_entrypoint(void)
{
        if (setuid(0) < 0) {
                fprintf(stderr, "exec_zzz-child: Failed to setuid to 0.\n");
                return -EFAULT;
        }

        return execl(zzz_path, NULL);
}

int main(void)
{
        pid_t child_pid = -1;
        
        if (geteuid()) {
                fprintf(stderr, "exec_zzz: Need root privilege.\n");
                return -EPERM;
        }

        if ((child_pid = fork()) < 0) {
                fprintf(stderr, "exec_zzz: Fork child process failed.\n");
                return -EFAULT;
        } else if (child_pid)
                return child_process_entrypoint();
        
        return 0;
}
