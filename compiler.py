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
            "    ;; --- sub --- ;;\n"
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

    def put(self):
        return (
            "    ;; --- put --- ;;\n"
            "    pop rax\n"
            "    sub rsp, 8\n"
            "    mov BYTE [rsp], al\n"
            "    xor rax, rax\n"
            "    mov rdx, 1\n" # count
            "    mov rsi, rsp\n" # buf
            "    mov rdi, 1\n" # stdout fd 1
            "    mov rax, 1\n" # syscall write
            "    syscall\n"
            "    add rsp, 8\n"
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
        return (
            "    ;; --- gt --- ;;\n"
            "    pop rax\n"
            "    pop rbx\n"
            "    cmp rbx, rax\n"
            "    mov rax, 0\n"
            "    mov rbx, 1\n"
            "    cmovg rax, rbx\n"
            "    push rax\n"
        )

    def lt(self):
        return (
            "    ;; --- lt --- ;;\n"
            "    pop rax\n"
            "    pop rbx\n"
            "    cmp rbx, rax\n"
            "    mov rax, 0\n"
            "    mov rbx, 1\n"
            "    cmovl rax, rbx\n"
            "    push rax\n"
        )

    def gte(self):
        return (
            "    ;; --- gte --- ;;\n"
            "    pop rax\n"
            "    pop rbx\n"
            "    cmp rbx, rax\n"
            "    mov rax, 0\n"
            "    mov rbx, 1\n"
            "    cmovge rax, rbx\n"
            "    push rax\n"
        )

    def lte(self):
        return (
            "    ;; --- lte --- ;;\n"
            "    pop rax\n"
            "    pop rbx\n"
            "    cmp rbx, rax\n"
            "    mov rax, 0\n"
            "    mov rbx, 1\n"
            "    cmovle rax, rbx\n"
            "    push rax\n"
        )

    def while_(self):
        return (
            f"    ;; --- while --- ;;\n"
            f"label_{self.current_token.position}:\n"
            f"    pop rax\n"
            f"    cmp rax, 0\n"
            f"    jz label_{self.current_token.value}\n"
        )

    def if_(self):
        return (
            f"    ;; --- if --- ;;\n"
            f"    pop rax\n"
            f"    cmp rax, 0\n"
            f"    jz label_{self.current_token.value}\n"
        )

    def else_(self):
        return (
            f"    ;; --- else --- ;;\n"
            f"    jmp label_{self.current_token.value}\n"
            f"label_{self.current_token.position}:\n"
        )

    def end(self):
        assembly = f"    ;; --- end --- ;;\n"
        if self.current_token.value:
            assembly += f"    jmp label_{self.current_token.value}\n"
        assembly += f"label_{self.current_token.position}:\n"
        return assembly

    def int_(self):
        return (
            f"    ;; --- {self.current_token.value} --- ;;\n"
            f"    push {self.current_token.value}\n"
        )

    def compile(self, tokens: list[Token], output_filename: str) -> None:
        assert TokenType.COUNT_OPS == 16, "Remember to update compilcation implementation"
        generate: dict[TokenType, function] = {
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
