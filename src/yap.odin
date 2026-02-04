package src

import "core:fmt"
import "core:os"

Token :: enum {
	Unkown,
	Dash,
	DoubleDash,
	Equal,
	EndOfLine,
	Word,
}

main :: proc() {

	filePath := os.args[1]
	fmt.printfln(">Reading %s", filePath)

	text, ok := os.read_entire_file(filePath)
	assert(ok)

	fmt.println(">File read with success")

	fmt.printfln("\n%s\n", text)

	fmt.println("\n... press RETURN to continue")
	buf: [1]u8
	n, err := os.read(os.stdin, buf[:])
}
