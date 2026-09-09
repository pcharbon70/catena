"""Inspect Python call syntax without importing or executing the scanned file."""
import ast
import hashlib
import json
import pathlib
import sys

source = pathlib.Path(sys.argv[1]).read_bytes()
if len(source) > 2_000_000:
    raise ValueError('trust source limit')
tree = ast.parse(source)
calls = []
for node in ast.walk(tree):
    if isinstance(node, ast.Call):
        text = ast.dump(node, annotate_fields=True, include_attributes=False)
        calls.append({'target': ast.unparse(node.func),
                      'expression': hashlib.sha256(text.encode('utf-8')).hexdigest()})
    elif isinstance(node, (ast.Import, ast.ImportFrom)):
        text = ast.dump(node, annotate_fields=True, include_attributes=False)
        calls.append({'target': 'import',
                      'expression': hashlib.sha256(text.encode('utf-8')).hexdigest()})
print(json.dumps(sorted(calls, key=lambda value: (value['target'], value['expression'])), separators=(',', ':')))
