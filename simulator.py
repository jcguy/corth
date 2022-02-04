from sys import stderr
from language import Token, TokenType


class Simulator:
    def __init__(self, debug: bool = False) -> None:
        self.stack: list[int] = []
        self.ip: int = 0
        self.current_token: Token = None
        self.debug = debug

    def debug_log(self, message: str) -> None:
        if not self.debug:
            return

        print(message, file=stderr)

    def pop(self) -> int:
        return self.stack.pop()

    def push(self, value: int) -> None:
        self.stack.append(value)

    def dump(self):
        a = self.pop()
        print(a)

    def add(self):
        a = self.pop()
        b = self.pop()
        self.push(b + a)

    def sub(self):
        a = self.pop()
        b = self.pop()
        self.push(b - a)

    def dup(self):
        a = self.pop()
        self.push(a)
        self.push(a)

    def put(self):
        a = self.pop()
        print(chr(a), end="")

    def swap(self):
        a = self.pop()
        b = self.pop()
        self.push(a)
        self.push(b)

    def over(self):
        a = self.pop()
        b = self.pop()
        self.push(b)
        self.push(a)
        self.push(b)

    def gt(self):
        a = self.pop()
        b = self.pop()
        if b > a:
            self.push(1)
        else:
            self.push(0)

    def lt(self):
        a = self.pop()
        b = self.pop()
        if b < a:
            self.push(1)
        else:
            self.push(0)

    def gte(self):
        a = self.pop()
        b = self.pop()
        if b >= a:
            self.push(1)
        else:
            self.push(0)

    def lte(self):
        a = self.pop()
        b = self.pop()
        if b <= a:
            self.push(1)
        else:
            self.push(0)

    def while_(self):
        a = self.pop()
        if not a:
            self.ip = self.current_token.value

    def if_(self):
        a = self.pop()
        if not a:
            self.ip = self.current_token.value

    def else_(self):
        self.ip = self.current_token.value

    def end(self):
        if not self.current_token.value:
            return
        self.ip = self.current_token.value - 1

    def int_(self):
        self.push(self.current_token.value)

    def simulate(self, tokens: list[Token]) -> None:
        assert TokenType.COUNT_OPS == 16, "Remember to update simulation implementation"
        implementations: dict[TokenType, function] = {
            TokenType.OP_DUMP: self.dump,
            TokenType.OP_ADD: self.add,
            TokenType.OP_SUB: self.sub,
            TokenType.OP_DUP: self.dup,
            TokenType.OP_PUT: self.put,
            TokenType.OP_SWAP: self.swap,
            TokenType.OP_OVER: self.over,
            TokenType.OP_GT: self.gt,
            TokenType.OP_LT: self.lt,
            TokenType.OP_GTE: self.gte,
            TokenType.OP_LTE: self.lte,
            TokenType.BLOCK_WHILE: self.while_,
            TokenType.BLOCK_IF: self.if_,
            TokenType.BLOCK_ELSE: self.else_,
            TokenType.BLOCK_END: self.end,
            TokenType.VALUE_INT: self.int_,
        }

        while self.ip < len(tokens):
            self.debug_log(f"current stack (ip:{self.ip}): {self.stack}")
            self.current_token = tokens[self.ip]
            self.debug_log(f"current token: {self.current_token}")
            try:
                func = implementations[self.current_token.type]
                self.debug_log(f"current function: {func.__name__}")
                func()
                self.ip += 1
            except KeyError:
                raise NotImplementedError(
                    f"No implementation found for {self.current_token}"
                )
            except TypeError as e:
                raise TypeError(f"Failed to simulate token {self.current_token}", e)
            except IndexError as e:
                raise IndexError(f"{self.current_token} did an index error", e)
