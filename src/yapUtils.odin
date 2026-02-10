package src

import "core:os"
FilterPath :: proc(FullPath: string) -> string {
	for i := len(FullPath) - 1; i >= 0; i -= 1 {
		if FullPath[i] == '/' {
			return FullPath[0:i + 1]
		}
	}
	return ""
}

FilterFileName_NoExtension :: proc(FullPath: string) -> string {
	high: i32 = 0
	low: i32 = i32(len(FullPath)) - 1

	for i: i32 = i32(len(FullPath)) - 1; i >= 0; i -= 1 {
		if FullPath[i] == '.' && high == 0 {
			high = i
		}
		if FullPath[i] == '/' {
			low = i + 1
			break
		}
	}
	if low < high {
		return FullPath[low:high]
	}
	return ""
}
