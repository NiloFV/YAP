package src

import "core:fmt"
import "core:os"

PRE_ALOCATED_MEM_MB :: 256

TokenType :: enum {
	Unkown,
	Dash,
	DoubleDash,
	Equal,
	EndOfLine,
	Word,
}

Token :: struct {
	Type:  TokenType,
	Value: []u8,
}

Lexer :: struct {
	Source: []u8,
	Tokens: [^]Token,
}

main :: proc() {

	filePath := os.args[1]
	fmt.printfln(">Reading %s", filePath)

	text, ok := os.read_entire_file(filePath)
	assert(ok, "Failed to read file")

	fmt.println(">File read with success")

	//fmt.printfln("\n%s\n", text)

	mem := os.heap_alloc(int(MEGABYTES(PRE_ALOCATED_MEM_MB)))
	arena: MemoryArena
	InitializeArena(&arena, cast(^u8)mem, uintptr(MEGABYTES(PRE_ALOCATED_MEM_MB)))

	lex: Lexer
	lex.Source = text
	lex.Tokens = PushMultipointer(&arena, Token, 2)
	Tokenize(lex.Source, lex.Tokens)

	fmt.println(lex.Tokens[0].Type)
	fmt.printfln("%s", lex.Tokens[0].Value)

	fmt.println("\n... press RETURN to continue")
	buf: [1]u8
	n, err := os.read(os.stdin, buf[:])
}

Tokenize :: proc(Source: []u8, Destination: [^]Token) {
	Destination[0] = {
		Type  = .Dash,
		Value = Source[0:5],
	}
	Destination[1] = {
		Type  = .DoubleDash,
		Value = Source[8:15],
	}
}
