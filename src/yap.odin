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
	Type:      TokenType,
	ValueLow:  i32,
	ValueHigh: i32,
}

Lexer :: struct {
	Source:     []u8,
	Tokens:     [^]Token,
	TokenCount: u32,
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
	lex.Tokens = PushMultipointer(&arena, Token, 1, 0)
	lex.TokenCount = 1
	Tokenize(&arena, &lex)

	for i: u32 = 1; i < lex.TokenCount; i += 1 {
		if lex.Tokens[i].Type == .EndOfLine {
			fmt.printfln("%s", lex.Tokens[i].Type)

		} else {
			fmt.printfln("%s %24s", lex.Tokens[i].Type, lex.Source[ lex.Tokens[i].ValueLow:  lex.Tokens[i].ValueHigh])
		}
	}

	fmt.println("\n... press RETURN to continue")
	buf: [1]u8
	n, err := os.read(os.stdin, buf[:])
}

Tokenize :: proc(Arena: ^MemoryArena, Lex: ^Lexer) {

	indexLow: int

	for i := 0; i < len(Lex.Source); i += 1 {
		currentCharacter := &Lex.Source[i]
		switch currentCharacter^ {
		case '-':
			TryFlushWordToken(Arena, Lex, .EndOfLine, indexLow, i)
			indexLow = i + 1
			lastTok := &Lex.Tokens[Lex.TokenCount - 1]
			if lastTok.Type == .Dash {
				lastTok.Type = .DoubleDash
				lastTok.ValueHigh += 1
			} else {
				PushToken(Arena, Lex, .Dash, i, i + 1)
			}
		case '=':
			TryFlushWordToken(Arena, Lex, .EndOfLine, indexLow, i)
			indexLow = i + 1
			PushToken(Arena, Lex, .Equal, i, i + 1)
		case '\n':
			indexLow = i + 1
			lastTok := &Lex.Tokens[Lex.TokenCount - 1]
			if lastTok.Type != .EndOfLine {
				PushToken(Arena, Lex, .EndOfLine, i, i + 1)
			}
		case ' ':
			TryFlushWordToken(Arena, Lex, .EndOfLine, indexLow, i)
			indexLow = i + 1
		case '\r':
			TryFlushWordToken(Arena, Lex, .EndOfLine, indexLow, i)
			indexLow = i + 1
		case '\t':
			indexLow = i + 1			
		}
	}
	TryFlushWordToken(Arena, Lex, .EndOfLine, indexLow, len(Lex.Source)-1)
}


TryFlushWordToken :: proc(
	Arena: ^MemoryArena,
	Lex: ^Lexer,
	Tok: TokenType,
	#any_int ContentLow: i32,
	#any_int ContentHigh: i32,
) {
	if ContentHigh > ContentLow {
		PushToken(Arena, Lex, .Word, ContentLow, ContentHigh)
	}
}

PushToken :: proc(
	Arena: ^MemoryArena,
	Lex: ^Lexer,
	Tok: TokenType,
	#any_int ContentLow: i32,
	#any_int ContentHigh: i32,
) {
	tk := PushStruct(Arena, Token, 0)
	tk.Type = Tok
	tk.ValueLow = ContentLow
	tk.ValueHigh = ContentHigh
	Lex.TokenCount += 1
}
