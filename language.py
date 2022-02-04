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
    OP_SWAP = op()
    OP_OVER = op()
    OP_GT = op()
    OP_LT = op()
    OP_GTE = op()
    OP_LTE = op()

    BLOCK_WHILE = op()
    BLOCK_IF = op()
    BLOCK_ELSE = op()
    BLOCK_END = op()

    VALUE_INT = op()
    COUNT_OPS = op()

    def __repr__(self) -> str:
        return f"{self.name}"


class Token:
    ops = {
        ".": TokenType.OP_DUMP,
        "+": TokenType.OP_ADD,
        "-": TokenType.OP_SUB,
        "dup": TokenType.OP_DUP,
        "swap": TokenType.OP_SWAP,
        "over": TokenType.OP_OVER,
        ">": TokenType.OP_GT,
        "<": TokenType.OP_LT,
        ">=": TokenType.OP_GTE,
        "<=": TokenType.OP_LTE,
    }

    blocks = {
        "while": TokenType.BLOCK_WHILE,
        "if": TokenType.BLOCK_IF,
        "else": TokenType.BLOCK_ELSE,
        "end": TokenType.BLOCK_END,
    }

    def __init__(self, word: str, filename: str, row: int, col: int):
        self.filename = filename
        self.row = row + 1
        self.col = col + 1
        self.value = None
        self.position = None

        assert TokenType.COUNT_OPS == 15, "Remember to update Token.ops"
        if word in Token.ops:
            self.type = Token.ops[word]
            self.value = None
            return

        assert TokenType.COUNT_OPS == 15, "Remember to update Token.blocks"
        if word in Token.blocks:
            self.type = Token.blocks[word]
            self.value = None
            return

        assert TokenType.COUNT_OPS == 15, "Remember to update Token values"
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
        def __init__(self, location: int, token: Token) -> None:
            self.location = location
            self.token = token

    blocks: list[Block] = []

    assert TokenType.COUNT_OPS == 15, "Remember to update block parsing"
    for i, token in enumerate(tokenize_file(filename)):
        match token.type:
            case TokenType.BLOCK_WHILE:
                token.position = i
                blocks.append(Block(i, token))
            case TokenType.BLOCK_IF:
                token.position = i
                blocks.append(Block(i, token))
            case TokenType.BLOCK_ELSE:
                # Close if block
                open = blocks.pop()
                assert (
                    open.token.type is TokenType.BLOCK_IF
                ), f"Can't close {open.token} with {token}"
                open.token.value = i

                # open else block
                token.position = i
                blocks.append(Block(i, token))
            case TokenType.BLOCK_END:
                open = blocks.pop()
                # If this is already set, it's because we're ending an if-else pair of blocks
                if open.token.value:
                    assert (
                        open.token.type is TokenType.BLOCK_ELSE
                    ), f"Someone set {open.token} and it wasn't {token}"

                open.token.value = i
                token.position = i

                if open.token.type is not TokenType.BLOCK_IF:
                    token.value = open.location
            case _:
                pass

        yield token

    if blocks:
        raise RuntimeError(f"{blocks.pop().token}: this block wasn't closed")
