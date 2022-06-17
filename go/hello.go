package main

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"os"
)

type token struct {
	kind    string
	lexeme  string
	line    int
	literal interface{}
}

func tokenize(str_src string) (output []token, err error) {
	src := []rune(str_src)
	start, curr, line := 0, 0, 1
	var tokens []token

	// General helpers
	add := func(k string, l interface{}) {
		t := token{
			kind:    k,
			lexeme:  string(src[start:curr]),
			line:    line,
			literal: l,
		}
		tokens = append(tokens, t)
	}
	at_end := func() bool { return curr >= len(src) }
	peek := func() rune {
		if at_end() {
			return '\x00'
		}
		return src[curr]
	}
	advance := func() rune { curr++; return src[curr-1] }
	match := func(c rune) bool {
		if at_end() || src[curr] != c {
			return false
		}
		curr++
		return true
	}

	for !at_end() {
		start = curr
		c := advance()

		switch c {
		case '(':
			add("LPAREN", nil)
		case ')':
			add("RPAREN", nil)
		case '[':
			add("LBRACK", nil)
		case ']':
			add("RBRACK", nil)
		case '{':
			add("LCURLY", nil)
		case '}':
			add("RCURLY", nil)
		case ',':
			add("COMMA", nil)
		case '.':
			add("DOT", nil)
		case '-':
			add("MINUS", nil)
		case '+':
			add("PLUS", nil)
		case ':':
			add("COLON", nil)
		case ';':
			add("SEMICOLON", nil)
		case '*':
			add("STAR", nil)
		case '/':
			add("SLASH", nil)
		case '!':
			if match('=') {
				add("BANG_EQ", nil)
			} else {
				add("BANG", nil)
			}
		case '=':
			if match('=') {
				add("EQ_EQ", nil)
			} else {
				add("EQ", nil)
			}
		case '<':
			if match('=') {
				add("LESS_EQ", nil)
			} else {
				add("LESS", nil)
			}
		case '>':
			if match('=') {
				add("GREATER_EQ", nil)
			} else {
				add("GREATER", nil)
			}
		case '#':
			for peek() != '\n' && !at_end() {
				advance()
			}
		case ' ', '\r', '\t':
			break
		case '\n':
			line++
		case '"':
			// read_str()
			for peek() != '"' && !at_end() {
				if peek() == '\n' {
					line++
					advance()
				}
			}
			if at_end() {
				return nil, errors.New("Unterminated string.")
			}
			advance()
			str := src[start+1 : curr-1]
			// for i, _ := range str {
			// 	if str[i] == '\\' {
			//     switch (str[i+1]) {
			//     case '\\':
			//       str[i] :=
			//     }
			// 	}
			// }
			add("STR", string(str))
		default:
			/*
			   if unicode.IsDigit(c) {
			     read_num()
			   } else if unicode.IsLetter(c) || c == '_' {
			     read_symbol()
			   } else {
			     return nil, errors.New("Unexpected char: '" + string(c) + "'")
			   }*/
		}
	}

	return tokens, nil
}

func main() {
	filename := flag.String("i", "", "")
	flag.Parse()

	repl := (*filename == "")
	reader := bufio.NewReader(os.Stdin)

	for mainLoop := true; mainLoop; mainLoop = repl {
		var s string

		if repl {
			fmt.Print("> ")
			s, _ = reader.ReadString('\n')
		} else {
			raw, _ := os.ReadFile(*filename)
			s = string(raw)
		}

		tokens, _ := tokenize(s)

		for _, e := range tokens {
			fmt.Print(e.kind + " " + e.lexeme + " ")
			fmt.Println(e.literal)
		}
	}
}
