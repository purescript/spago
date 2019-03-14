import subprocess, time, signal, sys, os, json

if len(sys.argv) < 3:
    print("Run with `python3 windows_test_wrapper.py 4 \'[\"some\", \"command\"]\'`")
    sys.exit(1)

n = int(sys.argv[1])
command = json.loads(sys.argv[2])

def signal_handler(signal, frame):
  time.sleep(1)
  print('Ctrl+C received in the wrapper script')

signal.signal(signal.SIGINT, signal_handler)

print("Wrapper script started")

subprocess.Popen(command)

time.sleep(n)

os.kill(signal.CTRL_C_EVENT, 0)
