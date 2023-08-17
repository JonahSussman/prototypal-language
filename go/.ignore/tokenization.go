package main

import (
	"fmt"
	"strconv"
	"unicode"
)

// --- TOKENIZATION SECTION ---

type token struct {
	kind    string
	lexeme  string
	line    int
	literal interface{}
}

func token_error(tok token, msg string) error {
	var lex string
	if tok.kind == "END_OF_FILE" {
		lex = "at EOF"
	} else {
		lex = tok.lexeme
	}
	return fmt.Errorf(fmt.Sprintf("[line %d] Error %s: %s", tok.line, lex, msg))
}

func tokenize(str_src string) (output []token, err error) {
	keywords := map[string]string{
		"and":    "AND",
		"do":     "DO",
		"elif":   "ELIF",
		"else":   "ELSE",
		"end":    "END",
		"false":  "FALSE",
		"fun":    "FUN",
		"if":     "IF",
		"let":    "LET",
		"nil":    "NIL",
		"or":     "OR",
		"return": "RETURN",
		"then":   "THEN",
		"true":   "TRUE",
		"while":  "WHILE",
	}

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
	peek_next := func() rune {
		if curr+1 >= len(src) {
			return '\x00'
		}
		return src[curr+1]
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
			for peek() != '"' && !at_end() {
				if peek() == '\n' {
					line++
				}
				advance()
			}

			if at_end() {
				return nil, fmt.Errorf("unterminated string")
			}
			advance()
			var str []rune

			// fmt.Println("Parsing string!")
			i := start + 1
			for i < curr-1 {
				if src[i] == '\\' {
					switch src[i+1] {
					case '\'':
						str = append(str, '\'')
						i++
					case '"':
						str = append(str, '"')
						i++
					case '\\':
						str = append(str, '\\')
						i++
					case '?':
						str = append(str, '?')
						i++
					case 'n':
						str = append(str, '\n')
						i++
					case 't':
						str = append(str, '\t')
						i++
					default:
						str = append(str, '\\')
					}
				} else {
					str = append(str, src[i])
				}
				i++

				// fmt.Printf("i is: %v\n", i)
				// fmt.Printf("s is: %v\n", str)
			}

			// fmt.Printf("i is: %v\n", i)
			// fmt.Printf("s is: %v\n", str)

			add("STR", string(str))
			// for peek() != '"' && !at_end() {
			// 	if peek() == '\n' {
			// 		line++
			// 	}
			// 	advance()
			// }
			// if at_end() {
			// 	return nil, fmt.Errorf("unterminated string")
			// }
			// advance()
			// var str []rune

			// for i, _ := range src[start+1 : curr-1] {
			// 	if src[i+1] == '\\' {
			// 		switch src[i+2] {
			// 		case '\'':
			// 			str = append(str, '\'')
			// 		case '"':
			// 			str = append(str, '"')
			// 		case '\\':
			// 			str = append(str, '\\')
			// 		case '?':
			// 			str = append(str, '?')
			// 		case 'n':
			// 			str = append(str, '\n')
			// 		case 't':
			// 			str = append(str, '\t')
			// 		default:
			// 			str = append(str, '\\')
			// 		}
			// 	} else {
			// 		str = append(str, src[i+1])
			// 	}
			// }

			// add("STR", string(str))
		default:
			if unicode.IsDigit(c) {
				for unicode.IsDigit(peek()) {
					advance()
				}
				if peek() == '.' && unicode.IsDigit(peek_next()) {
					advance()
				}
				for unicode.IsDigit(peek()) {
					advance()
				}
				res, e := strconv.ParseFloat(string(src[start:curr]), 64)
				if e != nil {
					return nil, e
				}
				add("NUM", res)

			} else if unicode.IsLetter(c) || c == '_' {
				for unicode.IsLetter(peek()) || unicode.IsDigit(peek()) || peek() == '_' {
					advance()
				}
				text := string(src[start:curr])
				kind := "SYM"
				if val, ok := keywords[text]; ok {
					kind = val
				}
				add(kind, text)
			} else {
				return nil, fmt.Errorf("Unexpected char: '" + string(c) + "'")
			}
		}
	}

	add("END_OF_FILE", "")

	return tokens, nil
}
