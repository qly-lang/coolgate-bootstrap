# Coolgate bootstrap parser

Parse a coolgate text format source file `.cg` and output cbor format.
Coolgate in cbor format can be read and compiled by gcoolgate bootstrap compiler.
Write compiler in any programming language that output Coolgate IR should output cbor and call gcoolgate
for executable binary generation. This tool is intend to quickly construct a Coolgate snippet for human
testing Coolgate features.

## Build
```shell
sbt assmbly
```

## Run
```shell
scripts/cgb file.cg
```
It will generate `file.cbor` at same location of `file.cg`. You can use `scripts/cbor2json.py` to convert
binary cbor file to human readable json.