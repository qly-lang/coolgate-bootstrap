#/usr/bin/env python3
import cbor2
import json
import sys

source = sys.argv[1]
target = sys.argv[1][:-5]+'.json' if sys.argv[1].endswith('.cbor') else sys.argv[1]+'.json'
with open(source, 'rb') as source_file:
    with open(target, 'w') as target_file:
        obj = cbor2.load(source_file)
        json.dump(obj, target_file, indent=2)