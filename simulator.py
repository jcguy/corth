from language import Token, TokenType


class Simulator:
    def __init__(self) -> None:
        self.stack = []
        self.ip = 0
        self.current_token = None

    def dump(self):
        a = self.stack.pop()
        print(a)

    def add(self):
        a = self.stack.pop()
        b = self.stack.pop()
        self.stack.append(b + a)

    def sub(self):
        a = self.stack.pop()
        b = self.stack.pop()
        self.stack.append(b - a)

    def dup(self):
        a = self.stack.pop()
        self.stack.append(a)
        self.stack.append(a)

    def while_(self):
        a = self.stack.pop()
        if not a:
            self.ip = self.current_token.value

    def end(self):
        self.ip = self.current_token.value - 1

    def int_(self):
        self.stack.append(self.current_token.value)

    def simulate(self, tokens: list[Token]) -> None:
        implementations = {
            TokenType.OP_DUMP: self.dump,
            TokenType.OP_ADD: self.add,
            TokenType.OP_SUB: self.sub,
            TokenType.OP_DUP: self.dup,
            TokenType.KEY_WHILE: self.while_,
            TokenType.KEY_END: self.end,
            TokenType.VALUE_INT: self.int_,
        }

        while self.ip < len(tokens):
            self.current_token = tokens[self.ip]
            try:
                implementations[self.current_token.type]()
                self.ip += 1
            except KeyError:
                raise NotImplementedError(
                    f"No implementation found for {self.current_token}"
                )
            except TypeError as e:
                raise TypeError(f"Failed to simulate token {self.current_token}", e)
