import argparse

from language import parse_blocks
from simulator import Simulator


def main():
    parser = argparse.ArgumentParser(description="Corth compiler")
    parser.add_argument("filename", type=str)
    args = parser.parse_args()

    tokens = list(parse_blocks(args.filename))
    simulator = Simulator()
    simulator.simulate(tokens)


if __name__ == "__main__":
    main()
