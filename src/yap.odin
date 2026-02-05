package src

import "core:fmt"
import "core:math"
import "core:os"


PRE_ALOCATED_MEM_MB :: 256

TokenType :: enum {
	Unkown,
	Dash,
	DoubleDash,
	Equal,
	Word,
	EndOfLine,
	EndOfFile,
}

Token :: struct {
	Type:        TokenType,
	using Block: TextBlock,
	Line:        i32,
	Column:      i32,
}

Lexer :: struct {
	Source:     []u8,
	Tokens:     [^]Token,
	TokenCount: u32,
	SceneCount: u32,
}

Parser :: struct {
	Lex:          ^Lexer,
	CurrentToken: u32,
}

ParseError :: enum {
	None,
	NoWorldBlockAfterDash,
	SceneWithNoLines,
	SceneWithNoName,
}

TextBlock :: struct {
	IndexLow:  i32,
	IndexHigh: i32,
}


SceneNode :: struct {
	Content:         string,
	Transitions:     [16]u32,
	TransitionCount: u32,
}

SceneGraph :: struct {
	SceneName: string,
	Nodes:     [^]SceneNode,
	NodeCount: u32,
}

YapFile :: struct {
	Scenes:     [^]SceneGraph,
	SceneCount: u32,
}


main :: proc() {

	filePath := os.args[1]
	fmt.printfln(">Reading %s", filePath)

	text, ok := os.read_entire_file(filePath)
	assert(ok, "Failed to read file")

	fmt.println(">File read with success")

	mem := os.heap_alloc(int(MEGABYTES(PRE_ALOCATED_MEM_MB)))
	arena: MemoryArena
	InitializeArena(&arena, cast(^u8)mem, uintptr(MEGABYTES(PRE_ALOCATED_MEM_MB)))

	lex: Lexer
	lex.Source = text
	lex.Tokens = PushMultipointer(&arena, Token, 1, 0)
	lex.TokenCount = 1
	Tokenize(&arena, &lex)

	PrintLexer(&lex)

	parser: Parser
	parser.Lex = &lex
	parser.CurrentToken = 1

	fileOutput: YapFile

	Parse(&arena, &parser, &fileOutput)
	

	for i: u32 = 0; i < fileOutput.SceneCount; i += 1 {
		PrintScene(&fileOutput.Scenes[i])
		fmt.println("-----------------")
	}

	if parser.CurrentToken == lex.TokenCount{
		fmt.println("\nFile parsed with success!")
	}else{
		fmt.println("\nFailed to parse file")
	}

	fmt.println("\n... press RETURN to continue")
	buf: [1]u8
	n, err := os.read(os.stdin, buf[:])
}

Tokenize :: proc(Arena: ^MemoryArena, Lex: ^Lexer) {

	indexLow: int
	line: i32
	column: i32

	for i := 0; i < len(Lex.Source); i += 1 {
		currentCharacter := &Lex.Source[i]
		switch currentCharacter^ {
		case '-':
			TryFlushWordToken(Arena, Lex, .EndOfLine, indexLow, i, line, column)
			indexLow = i + 1
			lastTok := &Lex.Tokens[Lex.TokenCount - 1]
			if lastTok.Type == .Dash {
				lastTok.Type = .DoubleDash
				lastTok.IndexHigh += 1
			} else {
				PushToken(Arena, Lex, .Dash, i, i + 1, line, column + 1)
			}
		case '=':
			TryFlushWordToken(Arena, Lex, .EndOfLine, indexLow, i, line, column)
			indexLow = i + 1
			lastTok := &Lex.Tokens[Lex.TokenCount - 1]
			PushToken(Arena, Lex, .Equal, i, i + 1, line, column + 1)
			if i == 0 || lastTok.Type == .EndOfLine {
				Lex.SceneCount += 1
			}
		case '\n':
			indexLow = i + 1
			lastTok := &Lex.Tokens[Lex.TokenCount - 1]
			if lastTok.Type != .EndOfLine {
				PushToken(Arena, Lex, .EndOfLine, i, i + 1, line, column)
			}
			column = -1
			line += 1
		case ' ':
			TryFlushWordToken(Arena, Lex, .EndOfLine, indexLow, i, line, column)
			indexLow = i + 1
		case '\r':
			TryFlushWordToken(Arena, Lex, .EndOfLine, indexLow, i, line, column)
			indexLow = i + 1
		case '\t':
			indexLow = i + 1
		}
		column += 1
	}
	TryFlushWordToken(Arena, Lex, .EndOfLine, indexLow, len(Lex.Source) - 1, line, column)
	PushToken(Arena, Lex, .EndOfFile, len(Lex.Source) - 1, len(Lex.Source) - 1, line, column)
}


TryFlushWordToken :: proc(
	Arena: ^MemoryArena,
	Lex: ^Lexer,
	Tok: TokenType,
	#any_int ContentLow: i32,
	#any_int ContentHigh: i32,
	#any_int Line: i32,
	#any_int Column: i32,
) {
	if ContentHigh > ContentLow {
		PushToken(Arena, Lex, .Word, ContentLow, ContentHigh, Line, Column)
	}
}

PushToken :: proc(
	Arena: ^MemoryArena,
	Lex: ^Lexer,
	Tok: TokenType,
	#any_int ContentLow: i32,
	#any_int ContentHigh: i32,
	#any_int Line: i32,
	#any_int Column: i32,
) {
	tk := PushStruct(Arena, Token, 0)
	tk.Type = Tok
	tk.IndexLow = ContentLow
	tk.IndexHigh = ContentHigh
	tk.Line = Line
	tk.Column = Column - (ContentHigh - ContentLow)
	Lex.TokenCount += 1
}

PrintLexer :: proc(Lex: ^Lexer) {

	fmt.printfln("=================\nScene count: %d", Lex.SceneCount)

	for i: u32 = 1; i < Lex.TokenCount; i += 1 {
		if Lex.Tokens[i].Type == .EndOfLine || Lex.Tokens[i].Type == .EndOfFile {
			fmt.printfln(
				"%s (%d %d)",
				Lex.Tokens[i].Type,
				Lex.Tokens[i].Line,
				Lex.Tokens[i].Column,
			)

		} else {
			fmt.printfln(
				"%s (%d %d) %24s",
				Lex.Tokens[i].Type,
				Lex.Tokens[i].Line,
				Lex.Tokens[i].Column,
				Lex.Source[Lex.Tokens[i].IndexLow:Lex.Tokens[i].IndexHigh],
			)
		}
	}
	fmt.println("=================")
}

/*
Grammar:

S = Scene* | e
Scene = _equal WordBlock SceneContent
SceneContent = Line* | Branch*
Line = _dash WordBlock
Branch = _doubleDash WordBlock SceneContent BranchEnd
BranchEnd = _endOfLine _doubleDash _endOfLine | _endOfLine _doubleDash _endOfFile
WordBlock = Word* WordBlockEnd 
Word = _word | _dash | _doubleDash | _equal 
WordBlockEnd = _endOfLine Keyword | _endOfFile
Keyword = _dash | _doubleDash | _equal
*/


PeekToken :: proc(ParserIn: ^Parser, Offset: u32 = 0) -> (Token, bool) {
	tokenIndex := ParserIn.CurrentToken + Offset
	if tokenIndex < ParserIn.Lex.TokenCount {
		return ParserIn.Lex.Tokens[tokenIndex], true
	}
	return {}, false
}


Parse :: proc(Arena: ^MemoryArena, ParserIn: ^Parser, Out: ^YapFile) {
	Out.SceneCount = ParserIn.Lex.SceneCount
	Out.Scenes = PushMultipointer(Arena, SceneGraph, Out.SceneCount)

	for i: u32 = 0; i < Out.SceneCount; i += 1 {
		ok, err := ParseScene(Arena, ParserIn, &Out.Scenes[i])
		if !ok {
			LogError(err, ParserIn.Lex, {}, 0, 0)
			break
		}
	}
}

ParseScene :: proc(
	Arena: ^MemoryArena,
	ParserIn: ^Parser,
	OutScene: ^SceneGraph,
) -> (
	bool,
	ParseError,
) {
	tok, ok := PeekToken(ParserIn)
	if ok {
		if tok.Type == .Equal {
			ParserIn.CurrentToken += 1
			block: TextBlock
			block.IndexLow = tok.IndexHigh
			if ParseWordBlock(ParserIn, &block) {
				OutScene.SceneName = string(ParserIn.Lex.Source[block.IndexLow:block.IndexHigh])
				return ParseSceneContent(Arena, ParserIn, OutScene), .None
			} else {
				return false, .SceneWithNoName
			}
		}
	}
	return false, .None
}


ParseSceneContent :: proc(Arena: ^MemoryArena, ParserIn: ^Parser, Out: ^SceneGraph) -> bool {
	prevNode: ^SceneNode

	for {
		ok, err, block := ParseLine(ParserIn)
		if err != .None {
			LogError(err, ParserIn.Lex, block, 0, 0)
			return false
		}
		if ok {
			node := PushStruct(Arena, SceneNode)
			node.Content = string(ParserIn.Lex.Source[block.IndexLow:block.IndexHigh])
			if prevNode != nil {
				prevNode.Transitions[prevNode.TransitionCount] = Out.NodeCount
				prevNode.TransitionCount += 1
			}
			Out.NodeCount += 1
			if Out.Nodes == nil {
				Out.Nodes = node
			}
			prevNode = node
		} else {
			return Out.NodeCount > 0
		}
	}
	return false
}

ParseLine :: proc(ParserIn: ^Parser) -> (bool, ParseError, TextBlock) {
	tok, ok := PeekToken(ParserIn)
	if tok.Type == .Dash {
		ParserIn.CurrentToken += 1
		tok, ok = PeekToken(ParserIn)
		if !ok {
			return false, .NoWorldBlockAfterDash, {}
		}
		block: TextBlock
		block.IndexLow = tok.IndexLow
		worldBlockParsed := ParseWordBlock(ParserIn, &block)
		if !worldBlockParsed {
			return false, .NoWorldBlockAfterDash, {}
		}
		assert(block.IndexHigh > block.IndexLow)
		return true, .None, block
	}
	return false, .None, {}
}

ParseWordBlock :: proc(ParserIn: ^Parser, Block: ^TextBlock) -> bool {
	if ParseWordBlockEnd(ParserIn) {
		return true
	}
	tok, peekOk := PeekToken(ParserIn)
	if !peekOk {
		return false
	}
	if tok.Type == .EndOfLine {
		Block.IndexHigh = math.max(Block.IndexHigh, tok.IndexHigh)
		ParserIn.CurrentToken += 1
		return ParseWordBlock(ParserIn, Block)
	}

	indexHigh, ok := ParseWord(ParserIn, 0)
	if ok {
		Block.IndexHigh = math.max(Block.IndexHigh, indexHigh)
		ParserIn.CurrentToken += 1
		return ParseWordBlock(ParserIn, Block)
	}
	return false
}

ParseWordBlockEnd :: proc(ParserIn: ^Parser) -> bool {
	tok, ok := PeekToken(ParserIn)
	if ok {
		if tok.Type == .EndOfFile {
			ParserIn.CurrentToken += 1
			return true
		} else if tok.Type == .EndOfLine {
			if ParseKeyWord(ParserIn, 1) {
				ParserIn.CurrentToken += 1
				return true
			}
		}
	}
	return false
}

ParseWord :: proc(ParserIn: ^Parser, TokenOffset: u32) -> (indexHigh: i32, success: bool) {
	tok, ok := PeekToken(ParserIn, TokenOffset)

	if ok {
		return tok.IndexHigh,
			(tok.Type == .Dash ||
				tok.Type == .DoubleDash ||
				tok.Type == .Equal ||
				tok.Type == .Word)
	}
	return 0, false
}

ParseKeyWord :: proc(ParserIn: ^Parser, TokenOffset: u32) -> bool {
	tok, ok := PeekToken(ParserIn, TokenOffset)
	if ok {
		return tok.Type == .Dash || tok.Type == .DoubleDash || tok.Type == .Equal

	}
	return false
}

LogError :: proc(Error: ParseError, Lex: ^Lexer, Block: TextBlock, Line: i32, Column: i32) {
	message: string
	switch Error {
	case .None:
	case .NoWorldBlockAfterDash:
		message = "Expected text after -"
	case .SceneWithNoLines:
		message = "Scene with no lines"
	case .SceneWithNoName:
		message = "Expected scene name after ="
	}
	if len(message) > 0 {
		fmt.printfln(
			"ERROR (%d, %d):%s\n%s",
			Line,
			Column,
			message,
			Lex.Source[Block.IndexLow:Block.IndexHigh],
		)
	}
}

PrintScene :: proc(Scene: ^SceneGraph) {
	fmt.printfln("Scene: %s. %d nodes", Scene.SceneName, Scene.NodeCount)
	for i: u32 = 0; i < Scene.NodeCount; i += 1 {		
		fmt.printfln("\n# %d: %s", i, Scene.Nodes[i].Content)
		for t: u32 = 0; t < Scene.Nodes[i].TransitionCount; t += 1 {
			fmt.printfln("  -> %d", Scene.Nodes[i].Transitions[t])
		}
	}
}
