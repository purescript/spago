import subprocess
import os.path


def fail(msg):
    """
    Print a message and exit
    """
    print(msg)
    exit(1)


def call(expected_code, command, failure_msg):
    """
    Try to run a command and exit if fails
    """
    res = subprocess.run(command, capture_output=True)
    if res.returncode != expected_code:
        print(failure_msg)
        print("Program output:")
        fail(res.stderr.decode('utf-8'))
    else:
       return res


def expect_success(command, *args):
    print('Expecting success from: "' + ' '.join(command) + '"')
    return call(0, command, *args)


def expect_failure(command, *args):
    print('Expecting failure from: "' + ' '.join(command) + '"')
    return call(1, command, *args)
