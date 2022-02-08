from typing import Generator
from enum import IntEnum, auto, unique


op_count = 0


@unique
class TokenType(IntEnum):
    OP_DUMP = auto()
    OP_ADD = auto()
    OP_SUB = auto()
    OP_DUP = auto()
    OP_PUT = auto()
    OP_SWAP = auto()
    OP_OVER = auto()
    OP_GT = auto()
    OP_LT = auto()
    OP_GTE = auto()
    OP_LTE = auto()

    BLOCK_WHILE = auto()
    BLOCK_DO = auto()
    BLOCK_IF = auto()
    BLOCK_ELSE = auto()
    BLOCK_END = auto()

    VALUE_INT = auto()

    def __repr__(self) -> str:
        return f"{self.name}"


class Token:
    ops = {
        ".": TokenType.OP_DUMP,
        "+": TokenType.OP_ADD,
        "-": TokenType.OP_SUB,
        "dup": TokenType.OP_DUP,
        "put": TokenType.OP_PUT,
        "swap": TokenType.OP_SWAP,
        "over": TokenType.OP_OVER,
        ">": TokenType.OP_GT,
        "<": TokenType.OP_LT,
        ">=": TokenType.OP_GTE,
        "<=": TokenType.OP_LTE,
    }

    blocks = {
        "while": TokenType.BLOCK_WHILE,
        "do": TokenType.BLOCK_DO,
        "if": TokenType.BLOCK_IF,
        "else": TokenType.BLOCK_ELSE,
        "end": TokenType.BLOCK_END,
    }

    def __init__(self, word: str, filename: str, row: int, col: int):
        self.filename = filename
        self.row = row + 1
        self.col = col
        self.value = None
        self.position = None

        assert len(TokenType) == 17, "Remember to update Token.ops"
        if word in Token.ops:
            self.type = Token.ops[word]
            self.value = None
            return

        assert len(TokenType) == 17, "Remember to update Token.blocks"
        if word in Token.blocks:
            self.type = Token.blocks[word]
            self.value = None
            return

        assert len(TokenType) == 17, "Remember to update Token values"
        try:
            self.value = int(word)
            self.type = TokenType.VALUE_INT
        except ValueError as e:
            raise e

    def __repr__(self) -> str:
        return f"{self.filename}:{self.row}:{self.col}: Token({self.type.name},{self.value})"


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
        yield (col, word.strip())


def tokenize_file(filename: str) -> Generator[Token, None, None]:
    with open(filename) as f:
        lines: list[str] = f.readlines()

    yield from (
        Token(word, filename, row + 1, col + 1)
        for (row, line) in enumerate(lines)
        for (col, word) in parse_line(line)
    )


def parse_blocks(filename: str) -> Generator[Token, None, None]:
    class Block:
        def __init__(self, location: int, token: Token) -> None:
            self.location = location
            self.token = token

    blocks: list[Block] = []

    assert len(TokenType) == 17, "Remember to update block parsing"
    for i, token in enumerate(tokenize_file(filename)):
        match token.type:
            case TokenType.BLOCK_WHILE:
                token.position = i
                blocks.append(Block(i, token))
            case TokenType.BLOCK_DO:
                # close while block
                open = blocks.pop()
                assert (
                    open.token.type is TokenType.BLOCK_WHILE
                ), f"Can't use {token} without {TokenType.BLOCK_WHILE}; found {open.token}"

                # open do block
                token.position = i
                token.value = open.location
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
                assert open.token.type in (
                    TokenType.BLOCK_DO,
                    TokenType.BLOCK_IF,
                    TokenType.BLOCK_ELSE,
                ), f"Trying to close block {open.token} with invalid {token}"
                # If this is already set, it's because we're ending an if-else pair of blocks
                if open.token.value:
                    assert open.token.type in (
                        TokenType.BLOCK_ELSE,
                        TokenType.BLOCK_DO,
                    ), f"Someone set {open.token} and it wasn't {token}"

                if open.token.type in (TokenType.BLOCK_ELSE, TokenType.BLOCK_IF):
                    pass
                else:
                    assert open.token.type is TokenType.BLOCK_DO
                    token.value = open.token.value

                open.token.value = i
                token.position = i

            case _:
                pass

        yield token

    if blocks:
        raise RuntimeError(f"{blocks.pop().token}: this block wasn't closed")


def typecheck(tokens: list[Token]) -> list[Token]:
    pass
