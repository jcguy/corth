import argparse
from typing import Generator
from enum import IntEnum, unique


op_count = 0


def op(reset=False) -> int:
    global op_count
    if reset:
        op_count = 0

    op_count += 1
    return op_count - 1


@unique
class TokenType(IntEnum):
    OP_DUMP = op(True)
    OP_ADD = op()
    OP_SUB = op()
    OP_DUP = op()

    KEY_WHILE = op()
    KEY_END = op()

    VALUE_INT = op()

    def __repr__(self) -> str:
        return f"{self.name}"


class Token:
    ops = {
        ".": TokenType.OP_DUMP,
        "+": TokenType.OP_ADD,
        "-": TokenType.OP_SUB,
        "dup": TokenType.OP_DUP,
    }

    keywords = {
        "while": TokenType.KEY_WHILE,
        "end": TokenType.KEY_END,
    }

    def __init__(self, word: str, filename: str, row: int, col: int):
        self.filename = filename
        self.row = row
        self.col = col

        if word in Token.ops:
            self.type = Token.ops[word]
            self.value = None
            return

        if word in Token.keywords:
            self.type = Token.keywords[word]
            self.value = None
            return

        try:
            self.value = int(word)
            self.type = TokenType.VALUE_INT
        except ValueError as e:
            raise e

    def __repr__(self) -> str:
        return f"{self.filename}:{self.row}:{self.col}:Token({self.type.name},{self.value})"


def parse_line(line: str) -> Generator[tuple[int, str], None, None]:
    start = False
    word = ""
    col = -1
    for i, c in enumerate(line):
        if not start and c.isspace():
            continue

        if not start:
            start = True
            col = i

        if start and c.isspace():
            yield (col + 1, word.strip())
            start = False
            word = ""

        word += c

    if start:
        yield (col + 1, word.strip())


def tokenize_file(filename: str) -> Generator[Token, None, None]:
    with open(filename) as f:
        lines: list[str] = f.readlines()

    yield from (
        Token(word, filename, row, col)
        for (row, line) in enumerate(lines)
        for (col, word) in parse_line(line)
    )


def parse_blocks(filename: str) -> Generator[Token, None, None]:
    class Block:
        def __init__(self, location, other_location, token) -> None:
            self.location = location
            self.other_location = other_location
            self.token = token

    blocks: list[Block] = []

    for i, token in enumerate(tokenize_file(filename)):
        match token.type:
            case TokenType.KEY_WHILE:
                blocks.append(Block(i, None, token))
            case TokenType.KEY_END:
                open = blocks.pop()
                open.other_location = i
                token.value = open.location
                open.token.value = i

        yield token
