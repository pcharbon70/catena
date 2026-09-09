"""Fixed POSIX filesystem/process helper. Packet-4 JSON, no shell evaluation."""
import base64, json, os, selectors, signal, stat, struct, subprocess, sys, tempfile, time
MAX_PACKET = 4_300_000

class CleanupUnconfirmed(Exception):
    pass

def read_exact(size):
    result = bytearray()
    while len(result) < size:
        chunk = os.read(0, size - len(result))
        if not chunk:
            raise EOFError()
        result.extend(chunk)
    return bytes(result)

def read_packet():
    size = struct.unpack('>I', read_exact(4))[0]
    if size > MAX_PACKET:
        raise ValueError('packet limit')
    return json.loads(read_exact(size))

def answer(value):
    data = json.dumps(value, separators=(',', ':')).encode('utf-8')
    sys.stdout.buffer.write(struct.pack('>I', len(data)) + data)
    sys.stdout.buffer.flush()

def filesystem(request):
    parts = request['path'].split('/')
    if not parts or any(p in ('', '.', '..') or '\x00' in p or '\\' in p for p in parts):
        return {'error': 'denied'}
    root = os.open(request['root'], os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
    directory = root
    leaf = None
    try:
        for component in parts[:-1]:
            child = os.open(component, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=directory)
            if directory != root:
                os.close(directory)
            directory = child
        flags = os.O_NOFOLLOW | os.O_NONBLOCK
        flags |= os.O_RDONLY if request['operation'] == 'read' else os.O_WRONLY | os.O_CREAT
        leaf = os.open(parts[-1], flags, 0o600, dir_fd=directory)
        info = os.fstat(leaf)
        if not stat.S_ISREG(info.st_mode) or info.st_nlink != 1:
            return {'error': 'denied'}
        if request['operation'] == 'read':
            data = os.read(leaf, request['count'])
            return {'bytes': base64.b64encode(data).decode('ascii')}
        data = base64.b64decode(request['data'], validate=True)
        os.ftruncate(leaf, 0)
        position = 0
        while position < len(data):
            position += os.write(leaf, data[position:])
        return {'unit': True}
    finally:
        if leaf is not None:
            os.close(leaf)
        if directory != root:
            os.close(directory)
        os.close(root)

def stop_group(process):
    try:
        os.killpg(process.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    process.wait(timeout=2)

def run_process(request):
    command = request['command']
    # A private executable snapshot is stable for this launch. Host-approved
    # programs remain trusted code, not an OS sandbox or a dependency closure.
    with tempfile.TemporaryDirectory(prefix='catena-environment-') as directory:
        target = os.path.join(directory, 'program')
        source = os.open(command['executable'], os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK)
        try:
            info = os.fstat(source)
            if not stat.S_ISREG(info.st_mode) or info.st_size > 16_777_216:
                return {'error': 'denied'}
            with open(target, 'xb') as output:
                remaining = 16_777_216
                while True:
                    chunk = os.read(source, min(65536, remaining + 1))
                    if not chunk:
                        break
                    remaining -= len(chunk)
                    if remaining < 0:
                        return {'error': 'limit'}
                    output.write(chunk)
        finally:
            os.close(source)
        os.chmod(target, 0o500)
        selector = selectors.DefaultSelector()
        process = subprocess.Popen([target] + command['arguments'], executable=target,
            cwd=command['cwd'], env=command['environment'], stdin=subprocess.PIPE,
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, start_new_session=True)
        output = bytearray()
        data = base64.b64decode(request['data'], validate=True)
        position = 0
        deadline = time.monotonic() + request['timeout_ms'] / 1000
        try:
            os.set_blocking(process.stdin.fileno(), False)
            os.set_blocking(process.stdout.fileno(), False)
            selector.register(0, selectors.EVENT_READ, 'control')
            selector.register(process.stdout, selectors.EVENT_READ, 'output')
            selector.register(process.stdin, selectors.EVENT_WRITE, 'input')
            output_open = True
            while output_open:
                if time.monotonic() >= deadline:
                    return {'error': 'timeout'}
                for key, _ in selector.select(min(0.05, max(0, deadline - time.monotonic()))):
                    if key.data == 'control':
                        # Any parent control packet, or EOF after owner death,
                        # means cancellation; no command text is evaluated.
                        os.read(0, 4096)
                        return {'error': 'cancelled'}
                    if key.data == 'input':
                        try:
                            position += os.write(process.stdin.fileno(), data[position:position + 65536])
                        except BrokenPipeError:
                            position = len(data)
                        if position == len(data):
                            selector.unregister(process.stdin)
                            process.stdin.close()
                    if key.data == 'output':
                        chunk = os.read(process.stdout.fileno(), min(65536, request['max_bytes'] - len(output) + 1))
                        if not chunk:
                            selector.unregister(process.stdout)
                            output_open = False
                        else:
                            output.extend(chunk)
                            if len(output) > request['max_bytes']:
                                return {'error': 'limit'}
            while process.poll() is None:
                if time.monotonic() >= deadline:
                    return {'error': 'timeout'}
                for key, _ in selector.select(0.01):
                    if key.data == 'control':
                        return {'error': 'cancelled'}
            return {'status': process.returncode, 'bytes': base64.b64encode(output).decode('ascii')}
        finally:
            try:
                stop_group(process)
                selector.close()
                if not process.stdin.closed:
                    process.stdin.close()
                process.stdout.close()
            except (OSError, subprocess.SubprocessError):
                raise CleanupUnconfirmed()

try:
    request = read_packet()
    if (os.name != 'posix' or not all(hasattr(os, name) for name in ('O_NOFOLLOW', 'O_DIRECTORY', 'O_NONBLOCK'))
            or os.open not in os.supports_dir_fd):
        response = {'error': 'unavailable'}
    elif request['service'] == 'filesystem':
        response = filesystem(request)
    elif request['service'] == 'process':
        response = run_process(request)
    else:
        response = {'error': 'invalid_request'}
except CleanupUnconfirmed:
    response = {'cleanup_unconfirmed': True}
except FileNotFoundError:
    response = {'error': 'not_found'}
except (PermissionError, NotADirectoryError):
    response = {'error': 'denied'}
except EOFError:
    sys.exit(0)
except (OSError, ValueError, KeyError, subprocess.SubprocessError):
    response = {'error': 'io_failure'}
answer(response)
