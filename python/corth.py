#!/usr/bin/env python
import argparse
from compiler import Compiler

from parser import parse_blocks, typecheck, Token
from simulator import Simulator


def main():
    parser = argparse.ArgumentParser(description="Corth compiler")
    parser.add_argument("filename", type=str)
    parser.add_argument("--debug", action="store_true")
    parser.add_argument("--simulate", action="store_true")
    parser.add_argument("--compile", action="store_true")
    args = parser.parse_args()

    filename = args.filename
    tokens: list[Token] = list(parse_blocks(filename))
    typecheck(tokens)

    if args.simulate:
        simulator = Simulator(debug=args.debug)
        simulator.simulate(tokens)

    if args.compile:
        compiler = Compiler(debug=args.debug)
        compiler.compile(tokens, filename.rsplit(".", 1)[0] + ".asm")


if __name__ == "__main__":
    main()
