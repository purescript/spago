import subprocess
import os.path
import signal
import time


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


# Credit: https://stackoverflow.com/questions/4789837/
def run_for(delay, command):
    """
    Run a command and kill it after a certain delay
    """
    print('Going to run this for {}s: "{}"'.format(delay, ' '.join(command)))
    with open(os.devnull, 'w') as FNULL:
        process = subprocess.Popen(command, stdout=FNULL, stderr=FNULL)
        time.sleep(delay)
        process.send_signal(signal.SIGINT)


def check_fixture(name):
    fixture = '../fixtures/' + name
    with open(name, 'r', encoding='utf-8') as result, \
         open(fixture, 'r', encoding='utf-8') as expected:
        res_str = result.read()
        exp_str = expected.read()
        if res_str != exp_str:
            diff_res = call(0, ['diff', name, fixture], "diff failed")
            fail("Failed to verify the fixture\n" + diff_res.stdout.decode('utf-8'))
        else:
            print('Successfully verified fixture for "{}"'.format(name))
