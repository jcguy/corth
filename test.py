#!/usr/bin/env python
import argparse
import subprocess
from subprocess import PIPE
from os import listdir
from os.path import join, isfile, exists
from pathlib import Path
from subprocess import CompletedProcess

TEST_DIR = "./tests"
CACHE_FILE = join(TEST_DIR, "results.txt")
FAILURE = "\033[91m"
END = "\033[0m"

cache: dict[str, str] = {}
failed = []


def load_cache():
    if cache:
        return

    if not exists(CACHE_FILE):
        Path(CACHE_FILE).touch()
    with open(CACHE_FILE, "r") as f:
        for line in f:
            if line.isspace():
                continue
            file, result = line.split(":", 1)
            cache[file] = result.removesuffix("\n")


def compare_results(filename: str, stdout: bytes, record: bool):
    load_cache()

    print(filename, end="\n\t")
    got = str(stdout)
    print(f"Got\t\t{got}", end="\n\t")
    if filename in cache and not record:
        if cache[filename] != got:
            print(FAILURE, end="")
            failed.append(filename)

        print(f"Expected\t{cache[filename]}", end="")
        print(END)

    elif not record:
        print(f"No cached input for comparison", end="\n\t")

    if record:
        print(f"Storing\t\t{stdout}", end="\n\t")
        with open(CACHE_FILE, "a") as f:
            f.write(filename)
            f.write(":")
            f.write(str(stdout))
            f.write("\n")

    print()


def run_simulation_tests(args):
    filenames: list[str] = args.filenames
    if "all" in filenames:
        filenames = [
            f
            for f in listdir(TEST_DIR)
            if isfile(join(TEST_DIR, f)) and f.endswith(".corth")
        ]

    for filename in filenames:
        process: CompletedProcess = subprocess.run(
            f"/usr/bin/python corth.py {join(TEST_DIR, filename)} --simulate {'' if args.debug else ''}".split(),
            stdout=PIPE,
        )

        compare_results(filename, process.stdout, args.record)

    if failed:
        print(FAILURE, end="")
    print(
        f"{len(filenames)} total, {len(filenames) - len(failed)} succeeded, {len(failed)} failed",
        end="",
    )
    print(END)

    if failed:
        exit(1)


def run_compilation_tests(args):
    filenames: list[str] = args.filenames
    if "all" in filenames:
        filenames = [
            f
            for f in listdir(TEST_DIR)
            if isfile(join(TEST_DIR, f)) and f.endswith(".corth")
        ]

    for filename in filenames:
        subprocess.check_call(
            f"/usr/bin/python corth.py {join(TEST_DIR, filename)} --compile".split(),
        )
        subprocess.check_call(
            f"nasm -felf64 {join(TEST_DIR, filename.rsplit('.', 1)[0] + '.asm')}".split(),
        )
        subprocess.check_call(
            f"ld {join(TEST_DIR, filename.rsplit('.', 1)[0] + '.o')} -o {join(TEST_DIR, filename.rsplit('.', 1)[0])} -g -F dwarf".split(),
        )

        process: CompletedProcess = subprocess.run(
            f"{join(TEST_DIR, filename.rsplit('.', 1)[0])}".split(),
            stdout=PIPE
        )

        compare_results(filename, process.stdout, args.record)

    if failed:
        print(FAILURE, end="")
    print(
        f"{len(filenames)} total, {len(filenames) - len(failed)} succeeded, {len(failed)} failed",
        end="",
    )
    print(END)

    if failed:
        exit(1)


def main():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers()

    parser_run = subparsers.add_parser("simulate")
    parser_run.add_argument("filenames", type=str, nargs="+")
    parser_run.add_argument("--record", action="store_true")
    parser_run.add_argument("--debug", action="store_true")
    parser_run.set_defaults(func=run_simulation_tests)

    parser_compile = subparsers.add_parser("compile")
    parser_compile.add_argument("filenames", type=str, nargs="+")
    parser_compile.add_argument("--record", action="store_true")
    parser_compile.set_defaults(func=run_compilation_tests)

    args = parser.parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
