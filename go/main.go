package main

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"os"
	"strconv"
	"unicode"
)

type (
	Exp interface {
		print()
	}

	Number  float64
	String  string
	Boolean bool
	Symbol  string
	List    []*Exp
	Special int64

	Thing struct {
		table map[Exp]*Exp
		thing *Exp
	}

	Call struct {
		name *Exp
		call List
	}

	Procedure struct {
		params, body List
		env          *Env
	}

	Primitve struct {
		fun func(*Env, List) *Exp
		tco bool
	}
)

func (n Number) print()  { fmt.Print(n) }
func (s String) print()  { fmt.Print(s) }
func (b Boolean) print() { fmt.Print(b) }
func (s Symbol) print()  { fmt.Print(s) }

var obmap map[string]*Exp

func intern(name string) *Exp {
	if val, ok := obmap[name]; ok {
		return val
	}
	sym := Exp(Symbol(name))
	obmap[name] = &sym
	return &sym
}

type Env struct {
	vars  map[*Exp]*Exp
	outer *Env
}

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

	return errors.New(fmt.Sprintf("[line %d] Error %s: %s", tok.line, lex, msg))
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
				return nil, errors.New("unterminated string")
			}
			advance()
			var str []rune

			for i, _ := range src[start+1 : curr-1] {
				if src[i+1] == '\\' {
					switch src[i+2] {
					case '\'':
						str = append(str, '\'')
					case '"':
						str = append(str, '"')
					case '\\':
						str = append(str, '\\')
					case '?':
						str = append(str, '?')
					case 'n':
						str = append(str, '\n')
					case 't':
						str = append(str, '\t')
					default:
						str = append(str, '\\')
					}
				} else {
					str = append(str, src[i+1])
				}
			}

			add("STR", string(str))
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
				return nil, errors.New("Unexpected char: '" + string(c) + "'")
			}
		}
	}

	add("END_OF_FILE", "")

	return tokens, nil
}

// --- PARSING SECTION ---

func parse_at_end(tks []token, idx *int) bool {
	return tks[*idx].kind == "END_OF_FILE"
}
func parse_check(tks []token, idx *int, kind string) bool {
	return !parse_at_end(tks, idx) && tks[*idx].kind == kind
}
func parse_advance(tks []token, idx *int) (res token) {
	if !parse_at_end(tks, idx) {
		(*idx)++
	}
	return tks[*idx-1]
}
func parse_consume(tks []token, idx *int, kind string, m string) (token, error) {
	if parse_check(tks, idx, kind) {
		return parse_advance(tks, idx), nil
	}
	return token{}, token_error(tks[*idx], m)
}
func parse_synchronize(tks []token, idx *int) {
	parse_advance(tks, idx)
	for !parse_at_end(tks, idx) {
		if tks[*idx-1].kind == "SEMICOLON" {
			return
		}

		switch tks[*idx].kind {
		case "DO", "WHILE", "IF", "FUN", "LET", "PRINT":
			return
		}
		parse_advance(tks, idx)
	}
}
func parse_match(tks []token, idx *int, k string) bool {
	if !parse_check(tks, idx, k) {
		return false
	}

	parse_advance(tks, idx)
	return true
}
func parse_multimatch(tks []token, idx *int, v []string) int {
	for i, e := range v {
		if parse_check(tks, idx, e) {
			parse_advance(tks, idx)
			return i
		}
	}

	return -1
}

func parse(tokens []token) (exps []*Exp, err error) {
	var idx int
	for true {
		var exp *Exp
		exp, err = parse_expr(tokens, &idx)

		if err != nil {
			return
		}

		exps = append(exps, exp)
	}

	return
}

func parse_expr(tks []token, idx *int) (ret *Exp, err error) {
	if parse_match(tks, idx, "DO") {
		ret, err = parse_do(tks, idx)
	} else if parse_match(tks, idx, "WHILE") {
		ret, err = parse_while(tks, idx)
	} else if parse_match(tks, idx, "IF") {
		ret, err = parse_if(tks, idx)
	} else if parse_match(tks, idx, "FUN") {
		ret, err = parse_fun(tks, idx)
	} else if parse_match(tks, idx, "LET") {
		ret, err = parse_let(tks, idx)
	} else if parse_match(tks, idx, "RETURN") {
		ret, err = parse_return(tks, idx)
	} else {
		ret, err = parse_assignment(tks, idx)
	}

	if err != nil {
		return nil, err
	}

	parse_match(tks, idx, "SEMICOLON")

	return
}

func parse_block(tks []token, idx *int, terms []string) (exp *Exp, err error) {
	var block List
	for parse_multimatch(tks, idx, terms) < 0 {
		exp, err = parse_expr(tks, idx)
		if err != nil {
			return nil, err
		}
		block = append(block, exp)
	}
	(*idx)--
	c := Exp(Call{intern("__do"), block})
	return &c, nil
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
		exprs, _ := parse(&tokens)

		for _, e := range tokens {
			fmt.Print(e.kind + " " + e.lexeme + " ")
			fmt.Println(e.literal)
		}
	}
}
