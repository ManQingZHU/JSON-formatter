package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"unicode"
)

func tokenization(text []byte) (tokens [][]byte) {
	var i, j int
	for i = 0; i < len(text); {
		switch {
		// extract string
		case text[i] == '"':
			slash := false
			terminal := false
			for j = i + 1; j < len(text); j++ {
				switch {
				case text[j] == '\\':
					slash = !slash
				case text[j] == '"' && slash == false:
					terminal = true
				case slash == true:
					slash = false
				}
				if terminal {
					break
				}
			}
			tokens = append(tokens, []byte{text[i]})
			tokens = append(tokens, text[i:j])
			tokens = append(tokens, []byte{text[j]})
			i = j + 1

		// extract number
		case text[i] == '-' || unicode.IsDigit(rune(text[i])):
			for j = i + 1; j < len(text); j++ {
				if text[j] == ']' || text[j] == '}' || text[j] == ',' || unicode.IsSpace(rune(text[j])) {
					break
				}
			}
			tokens = append(tokens, text[i:j])
			i = j

		case text[i] == 't' || text[i] == 'n':
			tokens = append(tokens, text[i:i+4])
			i += 4

		case text[i] == 'f':
			tokens = append(tokens, text[i:i+5])
			i += 5

		// other
		case !unicode.IsSpace(rune(text[i])):
			tokens = append(tokens, []byte{text[i]})
			i++

		// skip whitespace
		default:
			i++
		}
	}
	return
}

// change to a new line and add indent
func newLine(cnt int) {
	var str string
	for i := 0; i < cnt; i++ {
		str += "   "
	}
	fmt.Printf( "\n%v", str)
}

var replaceMap = map[byte][]byte{
	'<':  []byte("&lt;"),
	'>':  []byte("&gt;"),
	'&':  []byte("&amp;"),
	'\'': []byte("&apos;"),
	'"':  []byte("&quot;"),
}

func IsHex(b byte) bool {
	return (b >= '0' && b <= '9') || (b >= 'a' && b <= 'f') || (b >= 'A' && b <= 'F')
}

var colorCode = map[byte]string{
	'{':  "b7b7b7",
	'[':  "76b852",
	':':  "f1632a",
	',':  "000000",
	'#':  "3399cc", // number
	'"':  "59626a", // string
	'\\': "7d3f98", // escape
	'c':  "f48924", // true, false, nil
}

func printWithColor(char []byte) {
	fmt.Print("<span style=\"color:", colorCode[char[0]], "\">", string(char[1:]), "</span>")
}

func parseAndPrint(tokens [][]byte) {
	indent := 0
	scopeStack := make([]bool, 0)
	var char byte

	for i := 0; i < len(tokens); i++ {
		char = tokens[i][0]
		if len(tokens[i]) > 1 {
			switch char {
			case 't', 'f', 'n':
				printWithColor(append([]byte{'c'}, tokens[i]...))

			case '"':
				prev := 1
				for j := 1; j < len(tokens[i])-1; j++ {
					switch tokens[i][j] {
					case '<', '>', '&', '\'', '\\':
						// ordinary
						printWithColor(append([]byte{'"'}, tokens[i][prev:j]...))
						prev = j
						// replaced
						if tokens[i][j] != '\\' {
							printWithColor(append([]byte{'"'}, replaceMap[tokens[i][j]]...))
							prev = j + 1
						} else { // escape
							j++
							switch tokens[i][j] {
							case '"', '\\', '/', 'b', 'f', 'n', 'r', 't':
								printWithColor(append([]byte{'\\'}, tokens[i][prev:j+1]...))
								prev = j + 1
								continue
							case 'u':
								for j++; j < len(tokens[i])-1 && j-prev <= 5; j++ {
									if !IsHex(tokens[i][j]) {
										break
									}
								}
								if j-prev > 5 {
									printWithColor(append([]byte{'\\'}, tokens[i][prev:j+1]...))
									prev = j + 1
									continue
								}
							}
							printWithColor([]byte("\"\\"))
							j = prev
							prev = j + 1
						}
					}
				}
				printWithColor(append([]byte{'"'}, tokens[i][prev:]...))

			// number
			default:
				printWithColor(append([]byte{'#'}, tokens[i]...))
			}
		} else {
			switch char {
			case '{':
				printWithColor([]byte("{{"))
				indent++
				newLine(indent)
				scopeStack = append(scopeStack, true)

			case '}':
				indent--
				newLine(indent)
				printWithColor([]byte("{}"))
				scopeStack = scopeStack[:len(scopeStack)-1]

			case ':':
				printWithColor([]byte(": : "))

			case '[':
				printWithColor([]byte("[["))
				scopeStack = append(scopeStack, false)
				indent++
				if tokens[i+1][0] == '{' {
					newLine(indent)
				}

			case ']':
				if tokens[i-1][0] == '}' {
					newLine(indent)
				}
				printWithColor([]byte("[]"))
				scopeStack = scopeStack[:len(scopeStack)-1]
				indent--

			case ',':
				printWithColor([]byte(",,"))
				if scopeStack[len(scopeStack)-1] || tokens[i+1][0] == '[' || tokens[i-1][0] == ']' ||
					tokens[i-1][0] == '}' || tokens[i+1][0] == '{'{
					newLine(indent)
				}
			case '"':
				printWithColor(append([]byte{'"'}, replaceMap['"']...))
			// number
			default:
				printWithColor([]byte{'#', char})
			}
		}
	}
}

func main() {
	if len(os.Args) == 1 {
		log.Fatal("Need an input filename in the command line!")
	}

	filename := os.Args[1]
	data, err := ioutil.ReadFile(filename)
	if err != nil {
		log.Fatal(err)
	}

	tokens := tokenization(data)

	fmt.Printf("<html>\n<body style = \"background-color:fff9ea \">\n")
	fmt.Printf("<span style=\"font-family:monospace; white-space:pre \">")

	 parseAndPrint(tokens)

	 fmt.Printf("</span>\n</body>\n</html>")
}
