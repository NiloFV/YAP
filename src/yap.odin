package src

import "core:fmt"
import "core:math"
import "core:os"
import win32 "core:sys/windows"


PRE_ALOCATED_MEM_MB :: 256
I32_MAX :: 2147483647

Terminal :: enum {
	Unkown,
	Dash,
	DoubleDash,
	Equal,
	Word,
	EndOfLine,
	EndOfFile,
	AtSign,
	HashTag,
}

NonTerminal :: enum {
	Start,
	Scene,
	WordBlock,
	Word,
	WordBlockEnd,
	Keyword,
	SceneContent,
	Line,
	BranchBlock,
	Branch,
	BranchBlockEnd,
	ActorTag,
	WordLine,
	Marker,
}

Token :: struct {
	Type:        Terminal,
	using Block: TextBlock,
	Line:        i32,
	Column:      i32,
}

Lexer :: struct {
	Source:     []u8,
	Tokens:     [^]Token,
	TokenCount: i32,
	SceneCount: i32,
}

Parser :: struct {
	Lex:        ^Lexer,
	Arena:      ^MemoryArena,
	TokenIndex: i32,
}

ParseErrorType :: enum {
	None,
	NoSceneFound,
	NoWorldBlockAfterDash,
	SceneWithNoLines,
	SceneWithNoName,
	EmptyActorTag,
	MarkerWithNoName,
}

ParserError :: struct {
	Error:         ParseErrorType,
	IndexOnSource: i32,
	Line:          i32,
	Column:        i32,
}


TextBlock :: struct {
	IndexLow:  i32,
	IndexHigh: i32,
}

TreeNode :: struct {
	Children:      [^]^TreeNode,
	ChildrenCount: i32,
	Value:         TreeNodeContent,
}

TreeNodeContent :: union {
	SceneRootNodeData,
	LeafNodeData,
	NonTerminal,
	ParserError,
}

SceneRootNodeData :: struct {
	SceneName: string,
}

LeafNodeData :: struct {
	Content:  string,
	LeafType: YapFileLeafType,
}

YapOutputFlag :: enum {
	None,
	Binary,
	Csv,
}
YapOutputFlags :: bit_set[YapOutputFlag]

CompileSettings :: struct {
	OutputFlags: YapOutputFlags,
	Verbose:     b16,
	DebugLexer:  b16,
	DebugParser: b16,
	Path:        string,
	FileName:    string,
}

YAP_VERSION :: 1
YapCode := FileHeaderCode('Y', 'A', 'P', '!')

YapFileHeader :: struct #packed {
	MagicValue: u32,
	Version:    i32,
	SceneCount: i32,
}
YapFileScene :: struct #packed {
	SceneNameLenght: i32,
	ChildCount:      i32,
}
YapFileLeafType :: enum i32 {
	Unkown   = 0,
	Line     = 1,
	SetActor = 2,
	Marker   = 3,
}
YapFileLeaf :: struct #packed {
	ContentLenght:   i32,
	TransitionCount: i32,
	LeafType:        YapFileLeafType,
}

Arena: MemoryArena

main :: proc() {

	if os.args[1] == "-h" {
		fmt.println(HELP)
		return
	}

	filePath := os.args[1]

	text, ok := os.read_entire_file(filePath)
	assert(ok, "Failed to read file")

	settings: CompileSettings
	settings.OutputFlags = {.Binary}
	settings.Path = FilterPath(filePath)
	settings.FileName = FilterFileName_NoExtension(filePath)

	for i := 2; i < len(os.args); i += 1 {
		ProcessCompilationArg(os.args[i], &settings)
	}

	Compile(text, settings)

	fmt.println("\n... press RETURN to continue")
	buf: [1]u8
	n, err := os.read(os.stdin, buf[:])
}

HELP :: `
Usage: yap.exe myScript.yap (args[...])
args:
	-v 		: verbose, more logs and timming
	-debugLexer 	: print all tokens
	-debugParser 	: print parse tree
`

ProcessCompilationArg :: proc(Arg: string, Settings: ^CompileSettings) {
	if Arg[0] != '-' {
		return
	}
	pureArg := Arg[1:]
	switch pureArg {
	case "v":
		Settings.Verbose = true
	case "debugLexer":
		Settings.DebugLexer = true
	case "debugParser":
		Settings.DebugParser = true
	}
}


Compile :: proc(Source: []u8, Settings: CompileSettings) {
	mem := os.heap_alloc(int(MEGABYTES(PRE_ALOCATED_MEM_MB)))

	if Arena.Size == 0 {
		InitializeArena(&Arena, cast(^u8)mem, uintptr(MEGABYTES(PRE_ALOCATED_MEM_MB)))
	} else {
		ClearArena(&Arena)
	}

	perfCountFrequencyResult: win32.LARGE_INTEGER
	win32.QueryPerformanceFrequency(&perfCountFrequencyResult)
	perfCountFrequency: i64 = cast(i64)perfCountFrequencyResult
	counterStart: win32.LARGE_INTEGER
	win32.QueryPerformanceCounter(&counterStart)

	lex: Lexer
	lex.Source = Source
	lex.Tokens = PushMultipointer(&Arena, Token, 1, 0)
	lex.TokenCount = 1
	Tokenize(&Arena, &lex)

	if Settings.DebugLexer {
		PrintLexer(&lex)
	}

	parser: Parser = CreateParser(&lex, &Arena)

	root := Parse(&parser)
	hasParsingErrors: bool
	ParsePostProcess(root, &lex, &hasParsingErrors)

	if parser.TokenIndex == lex.TokenCount && !hasParsingErrors {

		fmt.printfln("\n=== File parsed with success ===")

		if YapOutputFlag.Binary in Settings.OutputFlags {
			buf: [1024]byte
			outputPath := fmt.bprintf(buf[:], "%s%s.yapb", Settings.Path, Settings.FileName)
			fileHandle, err := os.open(outputPath, os.O_CREATE | os.O_TRUNC | os.O_WRONLY)
			if err == nil {
				WriteYapFile(&parser, root, &fileHandle)
				os.close(fileHandle)
				fmt.printfln("\n--- Script exported to: %s ---", outputPath)
			}
		}

		if Settings.DebugParser {
			PrintParseTree(root, &lex)
		}

	} else {
		fmt.println("\n=== Failed to parse file ===")
	}

	if Settings.Verbose {
		counterEnd: win32.LARGE_INTEGER
		win32.QueryPerformanceCounter(&counterEnd)
		elapsedSeconds: f32 =
			(cast(f32)(counterEnd - counterStart)) / (cast(f32)perfCountFrequency)

		fmt.printfln(">> Version: %d", YAP_VERSION)
		fmt.printfln(">> Time elapsed: %f(s)", elapsedSeconds)
	}
}


CreateParser :: proc(Lex: ^Lexer, Arena: ^MemoryArena) -> Parser {
	parser: Parser
	parser.Lex = Lex
	parser.Arena = Arena
	parser.TokenIndex = 1

	return parser
}

Tokenize :: proc(Arena: ^MemoryArena, Lex: ^Lexer) {

	indexLow: int
	line: i32
	column: i32

	slashesFoundThisLine: i32 = 0

	for i := 0; i < len(Lex.Source); i += 1 {
		currentCharacter := &Lex.Source[i]

		skip := currentCharacter^ != '\n' && slashesFoundThisLine >= 2

		if skip == false {
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
				slashesFoundThisLine = 0
			case '@':
				TryFlushWordToken(Arena, Lex, .EndOfLine, indexLow, i, line, column)
				indexLow = i + 1
				PushToken(Arena, Lex, .AtSign, i, i + 1, line, column + 1)
			case '#':
				TryFlushWordToken(Arena, Lex, .EndOfLine, indexLow, i, line, column)
				indexLow = i + 1
				PushToken(Arena, Lex, .HashTag, i, i + 1, line, column + 1)
			case ' ':
				TryFlushWordToken(Arena, Lex, .EndOfLine, indexLow, i, line, column)
				indexLow = i + 1
			case '\r':
				TryFlushWordToken(Arena, Lex, .EndOfLine, indexLow, i, line, column)
				indexLow = i + 1
			case '\t':
				indexLow = i + 1
			case '/':
				slashesFoundThisLine += 1
			}
		}
		column += 1
	}
	TryFlushWordToken(Arena, Lex, .EndOfLine, indexLow, len(Lex.Source) - 1, line, column)
	PushToken(Arena, Lex, .EndOfFile, len(Lex.Source) - 1, len(Lex.Source) - 1, line, column)
}


TryFlushWordToken :: proc(
	Arena: ^MemoryArena,
	Lex: ^Lexer,
	Tok: Terminal,
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
	Tok: Terminal,
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

	for i: i32 = 1; i < Lex.TokenCount; i += 1 {
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

CreateParserError :: proc(error: ParseErrorType, tk: ^Token) -> ParserError {
	return ParserError {
		Error = error,
		Column = tk.Column,
		Line = tk.Line,
		IndexOnSource = tk.IndexHigh,
	}
}

LogError :: proc(Error: ParserError, Lex: ^Lexer) {
	message: string
	switch Error.Error {
	case .None:
	case .NoWorldBlockAfterDash:
		message = "Expected text after -"
	case .SceneWithNoLines:
		message = "Scene with no lines"
	case .SceneWithNoName:
		message = "Expected scene name after ="
	case .NoSceneFound:
		message = "Expected ="
	case .EmptyActorTag:
		message = "Expected text after @"
	case .MarkerWithNoName:
		message = "Expected word after #"

	}
	if len(message) > 0 {
		fmt.printfln(
			"ERROR (%d, %d):%s\n%s",
			Error.Line,
			Error.Column,
			message,
			Lex.Source[Error.IndexOnSource:Error.IndexOnSource + 4], //temp
		)
	}
}


/*
Grammar:

S = Scene*
Scene = _equal WordBlock SceneContent
SceneContent = Line* | ActorTag* | Marker*
Line = _dash WordBlock
WordBlock =  Word* WordBlockEnd
Word = _word | _dash | _doubleDash | _equal | _hashTag | _atSign
WordBlockEnd = _endOfLine Keyword | _endOfFile
WordLine = Word* _endOfLine
Keyword = _dash | _doubleDash | _equal
ActorTag = _atSign WordLine
Marker = _hashTag Word _endOfLine
*/

PeekToken :: proc(ParserIn: ^Parser, Offset: i32 = 0) -> (Token, bool) {
	tokenIndex := ParserIn.TokenIndex + Offset
	if tokenIndex < ParserIn.Lex.TokenCount {
		return ParserIn.Lex.Tokens[tokenIndex], true
	}
	return {}, false
}

MatchToken :: proc(ParserIn: ^Parser, TokenType: Terminal) -> (Token, bool) {
	tk, ok := PeekToken(ParserIn)
	if ok && tk.Type == TokenType {
		ParserIn.TokenIndex += 1
		return tk, true
	}
	return {}, false
}

CreateTreeNode :: proc(Arena: ^MemoryArena) -> ^TreeNode {
	node := PushStruct(Arena, TreeNode)
	node.Children = nil
	node.ChildrenCount = 0
	return node
}

Parse :: proc(ParserIn: ^Parser) -> ^TreeNode {
	return Start(ParserIn)
}

Start :: proc(ParserIn: ^Parser) -> ^TreeNode {
	root: ^TreeNode = PushStruct(ParserIn.Arena, TreeNode)
	root.Children = PushMultipointer(ParserIn.Arena, ^TreeNode, ParserIn.Lex.SceneCount)
	root.ChildrenCount = ParserIn.Lex.SceneCount
	root.Value = .Start

	for i: i32 = 0; i < ParserIn.Lex.SceneCount; i += 1 {
		tk, ok := PeekToken(ParserIn)
		if ok && tk.Type == .Equal {
			root.Children[i] = Scene(ParserIn)
		} else {
			errorNode := CreateTreeNode(ParserIn.Arena)
			errorNode.Value = CreateParserError(.NoSceneFound, &tk)
			root.Children[i] = errorNode
		}
	}
	return root
}

Scene :: proc(ParserIn: ^Parser) -> ^TreeNode {
	tk, ok := MatchToken(ParserIn, .Equal)
	assert(ok)
	sceneNameBlock := WordBlock(ParserIn)
	if sceneNameBlock.IndexLow >= sceneNameBlock.IndexHigh {
		errNode := CreateTreeNode(ParserIn.Arena)
		errNode.Value = CreateParserError(.SceneWithNoName, &tk)
		return errNode
	}
	sceneNode := CreateTreeNode(ParserIn.Arena)
	sceneNode.Value = SceneRootNodeData {
		SceneName = string(ParserIn.Lex.Source[sceneNameBlock.IndexLow:sceneNameBlock.IndexHigh]),
	}
	SceneContent(ParserIn, sceneNode)
	return sceneNode
}

SceneContent :: proc(ParserIn: ^Parser, SceneRoot: ^TreeNode) {
	nodes: [^]TreeNode

	for {
		tk, ok := PeekToken(ParserIn)
		if ok {
			node: ^TreeNode

			if tk.Type == .Dash || tk.Type == .AtSign || tk.Type == .HashTag {
				node = CreateTreeNode(ParserIn.Arena)
				SceneRoot.ChildrenCount += 1

				if nodes == nil {
					nodes = node
				}
			} else {
				break
			}

			#partial switch tk.Type {
			case .Dash:
				lineBlock := Line(ParserIn)
				if lineBlock.IndexLow >= lineBlock.IndexHigh {
					node.Value = CreateParserError(.NoWorldBlockAfterDash, &tk)
				} else {
					node.Value = LeafNodeData {
						Content  = string(
							ParserIn.Lex.Source[lineBlock.IndexLow:lineBlock.IndexHigh],
						),
						LeafType = .Line,
					}
				}
			case .AtSign:
				actorBlock := ActorTag(ParserIn)
				if actorBlock.IndexLow >= actorBlock.IndexHigh {
					node.Value = CreateParserError(.EmptyActorTag, &tk)
				} else {
					node.Value = LeafNodeData {
						Content  = string(
							ParserIn.Lex.Source[actorBlock.IndexLow:actorBlock.IndexHigh],
						),
						LeafType = .SetActor,
					}
				}
			case .HashTag:
				markerBlock := Marker(ParserIn)
				if markerBlock.IndexLow >= markerBlock.IndexHigh {
					node.Value = CreateParserError(.MarkerWithNoName, &tk)
				} else {
					node.Value = LeafNodeData {
						Content  = string(
							ParserIn.Lex.Source[markerBlock.IndexLow:markerBlock.IndexHigh],
						),
						LeafType = .Marker,
					}
				}
			}

		} else {
			break
		}
	}

	SceneRoot.Children = PushMultipointer(ParserIn.Arena, ^TreeNode, SceneRoot.ChildrenCount)
	for i: i32 = 0; i < SceneRoot.ChildrenCount; i += 1 {
		SceneRoot.Children[i] = &nodes[i]
	}
}


Line :: proc(ParserIn: ^Parser) -> TextBlock {
	tk, ok := MatchToken(ParserIn, .Dash)
	assert(ok)
	return WordBlock(ParserIn)
}

WordBlock :: proc(ParserIn: ^Parser) -> TextBlock {
	block: TextBlock
	block.IndexLow = I32_MAX
	block.IndexHigh = 0
	for {
		if WordBlockEnd(ParserIn) {
			break
		}

		tok, ok := MatchToken(ParserIn, .EndOfLine)
		if ok {
			continue
		}

		word := Word(ParserIn)
		if word.Type != .Unkown {
			block.IndexLow = math.min(block.IndexLow, word.IndexLow)
			block.IndexHigh = math.max(block.IndexHigh, word.IndexHigh)
		}
	}
	return block
}

WordLine :: proc(ParserIn: ^Parser) -> TextBlock {
	block: TextBlock
	block.IndexLow = I32_MAX
	block.IndexHigh = 0
	for {
		tok, ok := MatchToken(ParserIn, .EndOfLine)
		if ok {
			break
		}

		tok, ok = MatchToken(ParserIn, .EndOfFile)
		if ok {
			break
		}

		word := Word(ParserIn)
		if word.Type != .Unkown {
			block.IndexLow = math.min(block.IndexLow, word.IndexLow)
			block.IndexHigh = math.max(block.IndexHigh, word.IndexHigh)
		}
	}
	return block
}

Word :: proc(ParserIn: ^Parser) -> Token {
	tk, ok := PeekToken(ParserIn)
	if ok {
		if tk.Type == .Word ||
		   tk.Type == .Dash ||
		   tk.Type == .DoubleDash ||
		   tk.Type == .Equal ||
		   tk.Type == .AtSign ||
		   tk.Type == .HashTag {
			ParserIn.TokenIndex += 1
			return tk
		}
	}
	return {}
}


WordBlockEnd :: proc(ParserIn: ^Parser) -> bool {
	next, ok := PeekToken(ParserIn)
	if ok {
		if next.Type == .EndOfLine {
			ParserIn.TokenIndex += 1
			if Keyword(ParserIn) {
				return true
			} else {
				ParserIn.TokenIndex -= 1
			}
		} else {
			_, ok := MatchToken(ParserIn, .EndOfFile)
			return ok
		}
	}

	return false
}

ActorTag :: proc(ParserIn: ^Parser) -> TextBlock {
	next, ok := PeekToken(ParserIn)
	if ok {
		if next.Type == .AtSign {
			ParserIn.TokenIndex += 1
			block := WordLine(ParserIn)
			if block.IndexLow < block.IndexHigh {
				return block
			} else {
				ParserIn.TokenIndex -= 1
			}
		}
	}

	return {}
}

Marker :: proc(ParserIn: ^Parser) -> TextBlock {
	oldIndex := ParserIn.TokenIndex
	next, ok := PeekToken(ParserIn)
	if ok {
		if next.Type == .HashTag {
			ParserIn.TokenIndex += 1
			word := Word(ParserIn)
			if word.Type == .Word {
				_, endOfLineFound := MatchToken(ParserIn, .EndOfLine)
				if endOfLineFound {
					return word.Block
				}
			}
		}
	}
	ParserIn.TokenIndex = oldIndex
	return {}
}

Keyword :: proc(ParserIn: ^Parser) -> bool {
	tok, ok := PeekToken(ParserIn)
	if ok {
		return(
			tok.Type == .Dash ||
			tok.Type == .DoubleDash ||
			tok.Type == .Equal ||
			tok.Type == .AtSign ||
			tok.Type == .HashTag \
		)
	}
	return false
}

ParsePostProcess :: proc(Root: ^TreeNode, Lex: ^Lexer, Err: ^bool) {

	switch &node in Root.Value {
	case LeafNodeData:
	case SceneRootNodeData:
	case NonTerminal:
	case ParserError:
		LogError(node, Lex)
		Err^ = true
	}

	for i: i32 = 0; i < i32(Root.ChildrenCount); i += 1 {
		ParsePostProcess(Root.Children[i], Lex, Err)
	}
}

PrintParseTree :: proc(Root: ^TreeNode, Lex: ^Lexer) {

	switch node in Root.Value {
	case SceneRootNodeData:
		fmt.printfln("\nscene( %s )", node.SceneName)
	case LeafNodeData:
		switch node.LeafType {
		case .Unkown:
		case .Line:
			fmt.printfln("line( %s )", node.Content)
		case .SetActor:
			fmt.printfln("set actor( %s )", node.Content)
		case .Marker:
			fmt.printfln("marker( %s )", node.Content)
		}
	case NonTerminal:
		fmt.printfln("\n( %s )", node)
	case ParserError:
		LogError(node, Lex)
	}

	for i: i32 = 0; i < Root.ChildrenCount; i += 1 {
		PrintParseTree(Root.Children[i], Lex)
	}
}


FileHeaderCode :: #force_inline proc(a: rune, b: rune, c: rune, d: rune) -> u32 {
	return (cast(u32)a << 0) | (cast(u32)b << 8) | (cast(u32)c << 16) | (cast(u32)d << 24)
}

WriteYapFile :: proc(ParserIn: ^Parser, Root: ^TreeNode, FileHandle: ^os.Handle) {
	header: YapFileHeader
	header.MagicValue = YapCode
	header.Version = YAP_VERSION
	header.SceneCount = ParserIn.Lex.SceneCount

	writeErr: os.Error = nil
	_, writeErr = os.write_ptr(FileHandle^, &header, size_of(header))
	assert(writeErr == nil, "Failed to write header")

	nodeIndex: i32 = 0
	WriteYapFileRecursive(ParserIn, Root, FileHandle, &nodeIndex)
}

WriteYapFileRecursive :: proc(
	ParserIn: ^Parser,
	Root: ^TreeNode,
	FileHandle: ^os.Handle,
	NodeIndex: ^i32,
) {
	switch &node in Root.Value {
	case SceneRootNodeData:
		NodeIndex^ = 0

		scene: YapFileScene
		scene.SceneNameLenght = i32(len(node.SceneName))
		scene.ChildCount = Root.ChildrenCount
		sceneName := transmute([]u8)node.SceneName

		_, writeErr := os.write_ptr(FileHandle^, &scene, size_of(scene))
		assert(writeErr == nil, "Failed to write scene header")

		_, writeErr = os.write_ptr(FileHandle^, &sceneName[0], len(sceneName))
		assert(writeErr == nil, "Failed to write scene name")
	case LeafNodeData:
		line: YapFileLeaf

		content := transmute([]u8)node.Content
		line.ContentLenght = i32(len(node.Content))
		line.TransitionCount = 1
		line.LeafType = node.LeafType
		transitions: []i32 = {NodeIndex^ + 1}
		NodeIndex^ += 1

		_, writeErr := os.write_ptr(FileHandle^, &line, size_of(line))
		assert(writeErr == nil, "Failed to write line")
		_, writeErr = os.write_ptr(
			FileHandle^,
			&transitions[0],
			size_of(i32) * int(line.TransitionCount),
		)
		assert(writeErr == nil, "Failed to write line transitions")
		_, writeErr = os.write_ptr(FileHandle^, &content[0], len(content))
		assert(writeErr == nil, "Failed to write line content")

	case NonTerminal:
	case ParserError:
	}

	for i: i32 = 0; i < i32(Root.ChildrenCount); i += 1 {
		WriteYapFileRecursive(ParserIn, Root.Children[i], FileHandle, NodeIndex)
	}
}
