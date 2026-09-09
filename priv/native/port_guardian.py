"""Owned direct-child transport for the bounded eight-byte native Float ABI."""
import os
import selectors
import struct
import subprocess
import sys
import time

# The verified loader supplies an absolute private executable path and timeout.
executable, timeout_text = sys.argv[1:]
timeout = int(timeout_text) / 1000.0
child = subprocess.Popen([executable], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                         stderr=subprocess.DEVNULL, close_fds=True)
selector = selectors.DefaultSelector()
selector.register(sys.stdin.buffer, selectors.EVENT_READ, "owner")
selector.register(child.stdout, selectors.EVENT_READ, "child")
os.set_blocking(sys.stdin.fileno(), False)
os.set_blocking(child.stdout.fileno(), False)
os.set_blocking(child.stdin.fileno(), False)
sys.stdout.buffer.write(b"\x00\x01R")
sys.stdout.buffer.flush()
owner_buffer = bytearray()
child_buffer = bytearray()
pending = False
deadline = None
status = 0
close_requested = False
try:
    while True:
        wait = None if deadline is None else max(0, deadline - time.monotonic())
        events = selector.select(wait)
        if not events:
            status = 124
            break
        stop = False
        for key, _ in events:
            data = os.read(key.fileobj.fileno(), 10)
            if not data:
                status = 0 if key.data == "owner" else 125
                stop = True
                break
            if key.data == "owner":
                owner_buffer.extend(data)
                if len(owner_buffer) >= 2 and owner_buffer[:2] == b"\x00\x00":
                    close_requested = True
                    stop = True
                    break
                if len(owner_buffer) > 10 or (len(owner_buffer) >= 2 and owner_buffer[:2] != b"\x00\x08"):
                    status = 126
                    stop = True
                    break
                if len(owner_buffer) == 10:
                    if pending:
                        status = 126
                        stop = True
                        break
                    try:
                        written = os.write(child.stdin.fileno(), owner_buffer)
                    except (BlockingIOError, BrokenPipeError):
                        written = 0
                    if written != 10:
                        status = 126
                        stop = True
                        break
                    owner_buffer.clear()
                    pending = True
                    deadline = time.monotonic() + timeout
            else:
                child_buffer.extend(data)
                if len(child_buffer) > 10 or not pending or (len(child_buffer) >= 2 and child_buffer[:2] != b"\x00\x08"):
                    status = 126
                    stop = True
                    break
                if len(child_buffer) == 10:
                    sys.stdout.buffer.write(child_buffer)
                    sys.stdout.buffer.flush()
                    child_buffer.clear()
                    pending = False
                    deadline = None
        if stop:
            break
finally:
    # This guardian is the child's sole reaper. Popen.kill checks its own child;
    # no unowned OS PID is accepted from a request or used as authority.
    if child.poll() is None:
        child.kill()
    child.wait()
    selector.close()
    child.stdin.close()
    child.stdout.close()
if close_requested:
    sys.stdout.buffer.write(b"\x00\x01C")
    sys.stdout.buffer.flush()
sys.exit(status)
