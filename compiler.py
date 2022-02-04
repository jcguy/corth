from sys import stderr
from language import Token, TokenType


class Compiler:
    def __init__(self, debug: bool = False) -> None:
        self.current_token: Token = None
        self.debug = debug

    def debug_log(self, message: str) -> None:
        if not self.debug:
            return
        print(message, file=stderr)

    def preamble(self):
        return (
            ";; --- preamble --- ;;\n"
            "BITS 64\n"
            "segment .text\n"
            "dump:\n"
            "    mov     r9, -3689348814741910323\n"
            "    sub     rsp, 40\n"
            "    mov     BYTE [rsp+31], 10\n"
            "    lea     rcx, [rsp+30]\n"
            ".L2:\n"
            "    mov     rax, rdi\n"
            "    lea     r8, [rsp+32]\n"
            "    mul     r9\n"
            "    mov     rax, rdi\n"
            "    sub     r8, rcx\n"
            "    shr     rdx, 3\n"
            "    lea     rsi, [rdx+rdx*4]\n"
            "    add     rsi, rsi\n"
            "    sub     rax, rsi\n"
            "    add     eax, 48\n"
            "    mov     BYTE [rcx], al\n"
            "    mov     rax, rdi\n"
            "    mov     rdi, rdx\n"
            "    mov     rdx, rcx\n"
            "    sub     rcx, 1\n"
            "    cmp     rax, 9\n"
            "    ja      .L2\n"
            "    lea     rax, [rsp+32]\n"
            "    mov     edi, 1\n"
            "    sub     rdx, rax\n"
            "    xor     eax, eax\n"
            "    lea     rsi, [rsp+32+rdx]\n"
            "    mov     rdx, r8\n"
            "    mov     rax, 1\n"
            "    syscall\n"
            "    add     rsp, 40\n"
            "    ret\n"

            "global _start\n"
            "_start:\n"
        )

    def postscript(self):
        return (
            ";; --- postscript --- ;;\n"
            "    mov rax, 60\n"
            "    mov rdi, 0\n"
            "    syscall\n"
        )

    def dump(self):
        return (
            "    ;; --- dump --- ;;\n"
            "    pop rdi\n"
            "    call dump\n"
        )

    def add(self):
        return (
            "    ;; --- add --- ;;\n"
            "    pop rax\n"
            "    pop rbx\n"
            "    add rax, rbx\n"
            "    push rax\n"
        )

    def sub(self):
        return (
            "    ;; --- add --- ;;\n"
            "    pop rax\n"
            "    pop rbx\n"
            "    sub rbx, rax\n"
            "    push rbx\n"
        )

    def dup(self):
        return (
            "    ;; --- dup --- ;;\n"
            "    pop rax\n"
            "    push rax\n"
            "    push rax\n"
        )

    def swap(self):
        return (
            "    ;; --- swap --- ;;\n"
            "    pop rax\n"
            "    pop rbx\n"
            "    push rax\n"
            "    push rbx\n"
        )

    def over(self):
        return (
            "    ;; --- over --- ;;\n"
            "    pop rax\n"
            "    pop rbx\n"
            "    push rbx\n"
            "    push rax\n"
            "    push rbx\n"
        )

    def gt(self):
        return
        a = self.pop()
        b = self.pop()
        if b > a:
            self.push(1)
        else:
            self.push(0)

    def lt(self):
        return
        a = self.pop()
        b = self.pop()
        if b < a:
            self.push(1)
        else:
            self.push(0)

    def gte(self):
        return
        a = self.pop()
        b = self.pop()
        if b >= a:
            self.push(1)
        else:
            self.push(0)

    def lte(self):
        return
        a = self.pop()
        b = self.pop()
        if b <= a:
            self.push(1)
        else:
            self.push(0)

    def while_(self):
        return
        a = self.pop()
        if not a:
            self.ip = self.current_token.value

    def if_(self):
        return
        a = self.pop()
        if not a:
            self.ip = self.current_token.value

    def else_(self):
        return
        self.ip = self.current_token.value

    def end(self):
        return
        if not self.current_token.value:
            return
        self.ip = self.current_token.value - 1

    def int_(self):
        return (
            f";; --- {self.current_token.value} --- ;;\n"
            f"push {self.current_token.value}\n"
        )

    def compile(self, tokens: list[Token], output_filename: str) -> None:
        assert TokenType.COUNT_OPS == 15, "Remember to update simulation implementation"
        generate: dict[TokenType, function] = {
            TokenType.OP_DUMP: self.dump,
            TokenType.OP_ADD: self.add,
            TokenType.OP_SUB: self.sub,
            TokenType.OP_DUP: self.dup,
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

        assembly = self.preamble()
        for token in tokens:
            self.current_token = token
            self.debug_log(f"current token: {self.current_token}")

            try:
                assembly += generate[self.current_token.type]() or ''
            except KeyError:
                raise NotImplementedError(
                    f"No implementation found for {self.current_token}"
                )

        assembly += self.postscript()

        with open(output_filename, "w") as f:
            f.write(assembly)
