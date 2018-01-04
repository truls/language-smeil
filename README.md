# language-smeil

[![Build Status](https://travis-ci.org/truls/language-smeil.svg?branch=master)](https://travis-ci.org/truls/language-smeil)

SME Intermediate Language parser, pretty printer and utilities

## Building
Build using stack by running

```
stack build
```

This will build the `language-smeil` library and an executable named `smeast`
which can be run using

```
stack exec smeast -- <params>
```

or installed to a location (`~/.local/bin` is default on linux) using

```
stack install
```

## smeast
`smeast` is a tool for converting between the concrete syntax, JSON, or pretty
printed AST representations of SMEIL. Usage is as follows

```
smeast - SMEIL representation converter

Usage: smeast [-i|--input IN] [-o|--output OUT] (-f|--input-format ARG)
              (-g|--output-format ARG)
  Converts between different representations of SMEIL

Available options:
  -i,--input IN            Input file. Defaults to stdin.
  -o,--output OUT          Output file. Defaults to stdout
  -f,--input-format ARG    Format of input file. ARG must be one of choices
                           pretty-json json pretty or AST
  -g,--output-format ARG   Format of output file. ARG must be one of choices
                           pretty-json json pretty or AST
  -h,--help                Show this help text
```

For example, to parse concrete SMEIL syntax from a file and print it as pretty
formatted JSON use

```
stack exec smeast -- -i docs/langspec/samples/allops3.sme -f pretty -g pretty-json
```
