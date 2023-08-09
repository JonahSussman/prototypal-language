package main

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
)

type (
	Exp interface {
		as_string() string
		// eval(*Env) (*Exp, ReturnValue, error)
	}

	Number  float64
	String  string
	Boolean bool
	Symbol  string
	List    []*Exp
	Special string

	Thing struct {
		table map[Exp]*Exp
		meta  *Exp
	}

	Call struct {
		name *Exp
		args List
	}

	Procedure struct {
		params, body List
		env          *Env
	}

	Primitve struct {
		fun func(*Env, List, *Exp) (*Exp, ReturnValue, error)
		tco bool
	}

	ReturnValue struct {
		fun, val *Exp
	}
)

var special_nil *Exp
var special_top_scope *Exp

func (x Number) as_string() string  { return fmt.Sprintf("%v", x) }
func (x String) as_string() string  { return fmt.Sprintf("%s", x) }
func (x Boolean) as_string() string { return fmt.Sprintf("%v", x) }
func (x Symbol) as_string() string  { return fmt.Sprintf("%v", x) }

func (x List) as_string() string {
	var builder strings.Builder
	builder.WriteString("[")

	for i, e := range x {
		builder.WriteString((*e).as_string())
		if i != len(x)-1 {
			builder.WriteString(", ")
		}
	}

	builder.WriteString("]")
	return builder.String()
}

func (t Thing) as_string() string {
	var builder strings.Builder
	builder.WriteString("{ ")

	i := 0
	for k, v := range t.table {
		builder.WriteString(k.as_string())
		builder.WriteString(" = ")
		builder.WriteString((*v).as_string())
		if i < len(t.table)-1 {
			builder.WriteString(", ")
		}
		i++
	}

	builder.WriteString(" }")
	return builder.String()
}

func (c Call) as_string() string {
	var builder strings.Builder
	builder.WriteString((*c.name).as_string())
	builder.WriteString("(")

	for i, e := range c.args {
		builder.WriteString((*e).as_string())
		if i != len(c.args)-1 {
			builder.WriteString(", ")
		}
	}

	builder.WriteString(")")
	return builder.String()
}

func (x Primitve) as_string() string  { return fmt.Sprintf("<primitive: %v>", &x) }
func (x Procedure) as_string() string { return fmt.Sprintf("<procedure: %v>", &x) }
func (x Special) as_string() string   { return fmt.Sprintf("<special: %v>", x) }

// func (x *Number) as_string() string    { return (*x).as_string() }
// func (x *String) as_string() string    { return (*x).as_string() }
// func (x *Boolean) as_string() string   { return (*x).as_string() }
// func (x *Symbol) as_string() string    { return (*x).as_string() }
// func (x *List) as_string() string      { return (*x).as_string() }
// func (x *Thing) as_string() string     { return (*x).as_string() }
// func (x *Call) as_string() string      { return (*x).as_string() }
// func (x *Primitve) as_string() string  { return (*x).as_string() }
// func (x *Procedure) as_string() string { return (*x).as_string() }
// func (x *Special) as_string() string   { return (*x).as_string() }

func make_pexp(x Exp) *Exp { return &x }

var obmap map[string]*Exp

func intern(name string) *Exp {
	if val, ok := obmap[name]; ok {
		return val
	}
	sym := make_pexp(Symbol(name))
	obmap[name] = sym
	return sym
}

type Env struct {
	vars  map[(*Exp)](*Exp)
	outer *Env
}

func make_env(outer *Env, vars List, vals List) (*Env, error) {
	env := Env{}
	env.outer = outer
	err := env.zip(vars, vals)
	if err != nil {
		return nil, err
	}

	return &env, nil
}

func (env *Env) find(sym *Exp) bool {
	_, ok := env.vars[sym]
	return ok
}

func (env *Env) get(sym *Exp) (exp *Exp) {
	if exp, ok := env.vars[sym]; ok {
		return exp
	}

	if env.outer != nil {
		return env.outer.get(sym)
	}

	return nil
}

func (env *Env) zip(vars List, vals List) error {
	if len(vars) != len(vals) {
		return errors.New("Env.zip: Number of args does not match params.")
	}

	for i := 0; i < len(vars); i++ {
		env.vars[vars[i]] = vals[i]
	}

	return nil
}

func (env *Env) set(sym *Exp, exp *Exp) error {
	e := env

	for e != nil && !e.find(sym) {
		e = e.outer
	}

	if e == nil {
		return errors.New("Could not find symbol in env!")
	}

	e.vars[sym] = exp

	return nil
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
				return nil, errors.New("unterminated string")
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
			// 	return nil, errors.New("unterminated string")
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
				return nil, errors.New("Unexpected char: '" + string(c) + "'")
			}
		}
	}

	add("END_OF_FILE", "")

	return tokens, nil
}

// --- PARSING SECTION ---

// Parsing helper functions

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

// Parsing main section (order as shown in grammar.bnf)

func parse(tokens []token) (exps []*Exp, err error) {
	var idx int
	for !parse_at_end(tokens, &idx) {
		var exp *Exp
		exp, err = parse_expr(tokens, &idx)

		if err != nil {
			return nil, err
		}

		exps = append(exps, exp)
	}

	return exps, nil
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
	return make_pexp(Call{intern("__do"), block}), nil
}

func parse_do(tks []token, idx *int) (ret *Exp, err error) {
	ret, err = parse_block(tks, idx, []string{"END"})
	if err != nil {
		return nil, err
	}

	_, err = parse_consume(tks, idx, "END", "Expect 'end' after do block.")
	if err != nil {
		return nil, err
	}

	return ret, nil
}

func parse_while(tks []token, idx *int) (ret *Exp, err error) {
	ret, err = parse_expr(tks, idx)
	if err != nil {
		return nil, err
	}

	args := List{ret}

	parse_consume(tks, idx, "DO", "Expect 'do' after while condition.")
	if err != nil {
		return nil, err
	}

	ret, err = parse_block(tks, idx, []string{"END"})
	if err != nil {
		return nil, err
	}

	args = append(args, ret)

	parse_consume(tks, idx, "END", "Expect 'end' after while block.")
	if err != nil {
		return nil, err
	}

	return make_pexp(Call{intern("__while"), args}), nil
}

func parse_if(tks []token, idx *int) (ret *Exp, err error) {
	// return nil, token_error(tks[*idx], "'parse_if' not implemented.")
	x, err := parse_expr(tks, idx)
	if err != nil {
		return nil, err
	}

	args := List{x}

	_, err = parse_consume(tks, idx, "THEN", "Expect 'then' after if condition.")
	if err != nil {
		return nil, err
	}

	b, err := parse_block(tks, idx, []string{"ELIF", "ELSE", "END"})
	if err != nil {
		return nil, err
	}

	args = append(args, b)

	i := parse_multimatch(tks, idx, []string{"ELIF", "ELSE"})
	for i >= 0 {
		if i == 0 {
			e, err := parse_expr(tks, idx)
			if err != nil {
				return nil, err
			}
			args = append(args, e)

			_, err = parse_consume(tks, idx, "THEN", "Expect 'then' after elif condition.")
			if err != nil {
				return nil, err
			}

			b, err := parse_block(tks, idx, []string{"ELIF", "ELSE", "END"})
			if err != nil {
				return nil, err
			}

			args = append(args, b)
		} else {
			args = append(args, make_pexp(Boolean(true)))

			b, err := parse_block(tks, idx, []string{"END"})
			if err != nil {
				return nil, err
			}
			args = append(args, b)
		}

		i = parse_multimatch(tks, idx, []string{"ELIF", "ELSE"})
	}

	_, err = parse_consume(tks, idx, "END", "Expect 'end' after if block.")
	if err != nil {
		return nil, err
	}
	return make_pexp(Call{intern("__if"), args}), nil
}

func parse_fun(tks []token, idx *int) (ret *Exp, err error) {
	_, err = parse_consume(tks, idx, "LBRACK", "Expect '[' for fun parameter list.")
	if err != nil {
		return nil, err
	}

	var params List
	if !parse_check(tks, idx, "RBRACK") {
		for {
			tok, err := parse_consume(tks, idx, "SYM", "Expect Symbol in param list")
			if err != nil {
				return nil, err
			}
			params = append(params, intern(tok.literal.(string)))
			if !parse_match(tks, idx, "COMMA") {
				break
			}
		}
	}

	_, err = parse_consume(tks, idx, "RBRACK", "Expect ']' after parameter list.")
	if err != nil {
		return nil, err
	}

	e, err := parse_expr(tks, idx)
	if err != nil {
		return nil, err
	}

	return make_pexp(Call{intern("__fun"), List{make_pexp(params), e}}), nil
}

func parse_let(tks []token, idx *int) (ret *Exp, err error) {
	tok, err := parse_consume(tks, idx, "SYM", "Expect symbol.")
	if err != nil {
		return nil, err
	}

	ret = intern(tok.literal.(string))

	_, err = parse_consume(tks, idx, "EQ", "Expect '=' after identifier.")
	if err != nil {
		return nil, err
	}

	x, err := parse_expr(tks, idx)
	if err != nil {
		return nil, err
	}

	return make_pexp(Call{intern("__let"), List{ret, x}}), nil
}

func parse_return(tks []token, idx *int) (ret *Exp, err error) {
	e, err := parse_expr(tks, idx)
	if err != nil {
		return nil, err
	}
	return make_pexp(Call{intern("__return"), List{e}}), nil
}

func parse_assignment(tks []token, idx *int) (ret *Exp, err error) {
	ret, err = parse_lor(tks, idx)
	if err != nil {
		return nil, err
	}

	if parse_match(tks, idx, "EQ") {
		equals := tks[*idx-1]
		value, err := parse_expr(tks, idx)
		if err != nil {
			return nil, err
		}

		if _, ok := (*ret).(Symbol); ok {
			return make_pexp(Call{intern("__assign"), List{ret, value}}), nil
		} else if e, ok := (*ret).(Call); ok && e.name == intern("__get") {
			return make_pexp(Call{intern("__set"), List{e.args[0], e.args[1], value}}), nil
		}

		return nil, token_error(equals, "Invalid assignment target,")
	}

	return ret, nil
}

func parse_infix(
	tks []token,
	idx *int,
	next func([]token, *int) (ret *Exp, err error),
	ts []string,
) (ret *Exp, err error) {
	infix_map := map[string]string{
		"SLASH":      "__div",
		"STAR":       "__mul",
		"MINUS":      "__sub",
		"PLUS":       "__add",
		"GREATER":    "__gt",
		"GREATER_EQ": "__geq",
		"LESS":       "__lt",
		"LESS_EQ":    "__leq",
		"BANG_EQ":    "__neq",
		"EQ_EQ":      "__eq",
		"AND":        "__and",
		"OR":         "__or",
	}

	ret, err = next(tks, idx)
	if err != nil {
		return nil, err
	}

	i := parse_multimatch(tks, idx, ts)
	for i >= 0 {
		next_exp, err := next(tks, idx)
		if err != nil {
			return nil, err
		}

		exp := Exp(Call{intern(infix_map[ts[i]]), List{ret, next_exp}})
		ret = &exp

		i = parse_multimatch(tks, idx, ts)
	}

	return ret, nil
}

func parse_lor(tks []token, idx *int) (ret *Exp, err error) {
	return parse_infix(tks, idx, parse_land, []string{"OR"})
}

func parse_land(tks []token, idx *int) (ret *Exp, err error) {
	return parse_infix(tks, idx, parse_eqty, []string{"AND"})
}

func parse_eqty(tks []token, idx *int) (ret *Exp, err error) {
	return parse_infix(tks, idx, parse_comp, []string{"BANG_EQ", "EQ_EQ"})
}

func parse_comp(tks []token, idx *int) (ret *Exp, err error) {
	return parse_infix(tks, idx, parse_term, []string{"GREATER", "GREATER_EQ", "LESS", "LESS_EQ"})
}

func parse_term(tks []token, idx *int) (ret *Exp, err error) {
	return parse_infix(tks, idx, parse_fact, []string{"MINUS", "PLUS"})
}

func parse_fact(tks []token, idx *int) (ret *Exp, err error) {
	return parse_infix(tks, idx, parse_unary, []string{"SLASH", "STAR"})
}

func parse_unary(tks []token, idx *int) (ret *Exp, err error) {
	if parse_match(tks, idx, "BANG") {
		y, err := parse_unary(tks, idx)
		if err != nil {
			return nil, err
		}

		return make_pexp(Call{intern("__not"), List{y}}), nil
	}
	if parse_match(tks, idx, "MINUS") {
		y, err := parse_unary(tks, idx)
		if err != nil {
			return nil, err
		}

		return make_pexp(Call{intern("__neg"), List{y}}), nil
	}

	return parse_call(tks, idx)
}

func parse_call(tks []token, idx *int) (ret *Exp, err error) {
	ret, err = parse_primary(tks, idx)
	if err != nil {
		return nil, err
	}

	for {
		if parse_match(tks, idx, "LPAREN") {
			var args List
			if !parse_check(tks, idx, "RPAREN") {
				for {
					if len(args) >= 255 {
						return nil, token_error(tks[*idx], "Cannot have more than 255 args.")
					}

					app, err := parse_expr(tks, idx)
					if err != nil {
						return nil, err
					}

					args = append(args, app)
					if !parse_match(tks, idx, "COMMA") {
						break
					}
				}
			}

			_, err = parse_consume(tks, idx, "RPAREN", "Expected ')' after arguments.")
			if err != nil {
				return nil, err
			}

			ret = make_pexp(Call{ret, args})
		} else if parse_match(tks, idx, "DOT") {
			tok, err := parse_consume(tks, idx, "SYM", "Expected field after '.'")
			if err != nil {
				return nil, err
			}

			sym := String(tok.literal.(string))
			ret = make_pexp(Call{intern("__get"), List{ret, make_pexp(sym)}})
		} else if parse_match(tks, idx, "LBRACK") {
			x, err := parse_expr(tks, idx)
			if err != nil {
				return nil, err
			}

			c := make_pexp(Call{intern("__get"), List{ret, x}})
			_, err = parse_consume(tks, idx, "RBRACK", "Expected ']' after field access.")
			if err != nil {
				return nil, err
			}

			ret = c
		} else if parse_match(tks, idx, "COLON") {
			tok, err := parse_consume(tks, idx, "SYM", "Expected Symbol after ':'")
			if err != nil {
				return nil, err
			}
			_, err = parse_consume(tks, idx, "LPAREN", "Expected '(' after selfcall.")
			if err != nil {
				return nil, err
			}

			var args List
			if !parse_check(tks, idx, "RPAREN") {
				for {
					if len(args) >= 255 {
						return nil, token_error(tks[*idx], "Cannot have more than 255 args.")
					}
					e, err := parse_expr(tks, idx)
					if err != nil {
						return nil, err
					}
					args = append(args, e)
					if !parse_match(tks, idx, "COMMA") {
						break
					}
				}
			}
			_, err = parse_consume(tks, idx, "RPAREN", "Expected ')' after arguments.")
			if err != nil {
				return nil, err
			}
			return make_pexp(Call{intern("__selfcall"), List{ret, make_pexp(String(tok.literal.(string))), make_pexp((args))}}), nil
		} else {
			break
		}
	}

	return ret, nil
}

func parse_primary(tks []token, idx *int) (ret *Exp, err error) {
	if parse_match(tks, idx, "TRUE") {
		return make_pexp(Boolean(true)), nil
	}
	if parse_match(tks, idx, "FALSE") {
		return make_pexp(Boolean(false)), nil
	}
	if parse_match(tks, idx, "NIL") {
		return special_nil, nil
	}
	if parse_match(tks, idx, "NUM") {
		if num, ok := tks[*idx-1].literal.(float64); ok {
			return make_pexp(Number(num)), nil
		}
		return nil, token_error(tks[*idx], "Mismatched literal and token values.")
	}
	if parse_match(tks, idx, "STR") {
		if str, ok := tks[*idx-1].literal.(string); ok {
			return make_pexp(String(str)), nil
		}
		return nil, token_error(tks[*idx], "Mismatched literal and token values.")
	}

	if parse_match(tks, idx, "SYM") {
		if sym, ok := tks[*idx-1].literal.(string); ok {
			return intern(sym), nil
		}
		return nil, token_error(tks[*idx], "Mismatched literal and token values.")
	}
	if parse_match(tks, idx, "LCURLY") {
		return parse_thing(tks, idx)
	}
	if parse_match(tks, idx, "LBRACK") {
		return parse_list(tks, idx)
	}
	if parse_match(tks, idx, "LPAREN") {
		x, err := parse_expr(tks, idx)
		if err != nil {
			return nil, err
		}

		_, err = parse_consume(tks, idx, "RPAREN", "Expect ')' after grouping expression.")
		if err != nil {
			return nil, err
		}

		return x, nil
	}

	return nil, token_error(tks[*idx], "Unexpected expression.")
}

func parse_thing(tks []token, idx *int) (ret *Exp, err error) {
	var fields List
	if !parse_check(tks, idx, "LCURLY") {
		i := 0

		for {
			if parse_match(tks, idx, "LBRACK") {
				e, err := parse_expr(tks, idx)
				if err != nil {
					return nil, err
				}
				fields = append(fields, e)

				_, err = parse_consume(tks, idx, "RBRACK", "Expect ']' after bracket field.")
				if err != nil {
					return nil, err
				}
				_, err = parse_consume(tks, idx, "EQ", "Expect '=' after field.")
				if err != nil {
					return nil, err
				}
			} else if parse_match(tks, idx, "DOT") {
				tok, err := parse_consume(tks, idx, "SYM", "Expected Symbol in dot field.")
				if err != nil {
					return nil, err
				}
				fields = append(fields, intern(tok.literal.(string)))
				_, err = parse_consume(tks, idx, "EQ", "Expect '=' after field.")
				if err != nil {
					return nil, err
				}
			} else if parse_check(tks, idx, "RCURLY") {
				break
			} else {
				fields = append(fields, make_pexp(Number(i)))
				i++
			}

			e, err := parse_expr(tks, idx)
			if err != nil {
				return nil, err
			}
			fields = append(fields, e)

			if !parse_match(tks, idx, "COMMA") {
				break
			}
		}
	}

	_, err = parse_consume(tks, idx, "RCURLY", "Expect '}' after field list.")
	if err != nil {
		return nil, err
	}

	return make_pexp(Call{intern("__thing"), fields}), nil

}

func parse_list(tks []token, idx *int) (ret *Exp, err error) {
	var elems List

	if !parse_check(tks, idx, "RBRACK") {
		for {
			e, err := parse_expr(tks, idx)
			if err != nil {
				return nil, err
			}

			elems = append(elems, e)

			if !parse_match(tks, idx, "COMMA") {
				break
			}
		}
	}

	_, err = parse_consume(tks, idx, "RBRACK", "Expect ']' after list expression.")
	if err != nil {
		return nil, err
	}

	return make_pexp(Call{intern("__list"), elems}), nil
}

// --- EVALUATION SECTION ---

func eval_through_tco(env *Env, list List, catch *Exp) (*Exp, ReturnValue, error) {
	if len(list) == 0 {
		return special_nil, ReturnValue{}, nil
	}

	for i := 0; i < len(list)-1; i++ {
		_, rval, err := eval(env, list[i], catch)
		if err != nil {
			return nil, ReturnValue{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}
	}

	return list[len(list)-1], ReturnValue{}, nil
}

func eval_list(env *Env, list List, catch *Exp) (List, ReturnValue, error) {
	for i, _ := range list {
		item, rval, err := eval(env, list[i], catch)

		if err != nil {
			return nil, ReturnValue{}, err
		}

		if rval.fun != nil {
			return nil, rval, nil
		}

		list[i] = item
	}

	return list, ReturnValue{}, nil
}

func eval(env *Env, exp *Exp, upper_catch *Exp) (immediate *Exp, return_value ReturnValue, rerr error) {
	for {
		// Really ugly
		_, is_number := (*exp).(Number)
		_, is_string := (*exp).(String)
		_, is_boolean := (*exp).(Boolean)
		_, is_thing := (*exp).(Thing)
		_, is_primitive := (*exp).(Primitve)
		_, is_procedure := (*exp).(Procedure)
		_, is_special := (*exp).(Special)
		is_self_evaluating := is_number || is_string || is_boolean || is_thing || is_primitive || is_procedure || is_special
		if is_self_evaluating {
			return exp, ReturnValue{}, nil
		}

		if sym, is_symbol := (*exp).(Symbol); is_symbol {
			bind := env.get(exp)
			if bind == nil {
				return nil, ReturnValue{}, errors.New("eval: Undefined symbol '" + string(sym) + "'.")
			}
			return bind, ReturnValue{}, nil
		}

		if call, is_call := (*exp).(Call); is_call {
			fun, rval, err := eval(env, call.name, upper_catch)
			if err != nil {
				return nil, ReturnValue{}, err
			}
			if rval.fun != nil {
				return nil, rval, nil
			}

			arg := call.args

			primitive, fun_is_primitive := (*fun).(Primitve)
			procedure, fun_is_procedure := (*fun).(Procedure)

			if !fun_is_primitive && !fun_is_procedure {
				return nil, ReturnValue{}, errors.New("eval: Call must be PRIMITIVE or PROCEDURE.")
			}

			if fun_is_procedure {
				this_catch := make_pexp(Special(fmt.Sprintf("procedure %v", fun)))

				farg, rval, err := eval_list(env, arg, this_catch)
				if err != nil {
					return nil, ReturnValue{}, err
				}
				if rval.fun != nil {
					if rval.fun == this_catch {
						return rval.val, ReturnValue{}, nil
					}

					return nil, rval, nil
				}

				fenv := procedure.env

				new_env, err := make_env(fenv, procedure.params, farg)
				if err != nil {
					return nil, ReturnValue{}, nil
				}

				env = new_env

				texp, rval, err := eval_through_tco(env, procedure.body, this_catch)
				if err != nil {
					return nil, ReturnValue{}, err
				}
				if rval.fun != nil {
					if rval.fun == this_catch {
						return rval.val, ReturnValue{}, nil
					}

					return nil, rval, nil
				}

				exp = texp

				// TCO return handling. VERY UGLY

				if c, is_call := (*exp).(Call); is_call {
					if s, is_symbol := (*c.name).(Symbol); is_symbol && string(s) == "__return" {
						e, rval, err := eval(env, c.args[0], this_catch)
						if err != nil {
							return nil, ReturnValue{}, err
						}
						if rval.fun != nil {
							if rval.fun == this_catch {
								return rval.val, ReturnValue{}, nil
							}

							return nil, rval, nil
						}

						return e, ReturnValue{}, nil
					}
				}
			} else {
				exp, rval, err = primitive.fun(env, arg, upper_catch)
				if err != nil {
					return nil, ReturnValue{}, nil
				}
				if rval.fun != nil {
					return nil, rval, nil
				}
				if !primitive.tco {
					return exp, ReturnValue{}, nil
				}
			}
		}

		return nil, ReturnValue{}, errors.New("eval: Unknown exp type.")
	}
}

func standard_env() *Env {
	env := Env{make(map[*Exp]*Exp), nil}

	add := func(name string, tco bool, fun func(env *Env, args List, catch *Exp) (*Exp, ReturnValue, error)) {
		env.vars[intern(name)] = make_pexp(Primitve{fun, tco})
	}

	// --- IO ---

	// print(expr, ...) -> nil
	//   expr :: The expressions to print

	add("print", false, func(env *Env, args List, catch *Exp) (*Exp, ReturnValue, error) {
		list, rval, err := eval_list(env, args, catch)
		if err != nil {
			return nil, ReturnValue{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}
		for _, e := range list {
			fmt.Print((*e).as_string())
		}

		return special_nil, ReturnValue{}, nil
	})

	// println(expr, ...) -> nil
	//   expr :: The expressions to print, then a single newline

	add("println", false, func(env *Env, args List, catch *Exp) (*Exp, ReturnValue, error) {
		list, rval, err := eval_list(env, args, catch)
		if err != nil {
			return nil, ReturnValue{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}
		for _, e := range list {
			fmt.Print((*e).as_string())
		}
		fmt.Println()

		return special_nil, ReturnValue{}, nil
	})

	// input(expr, ...) -> str
	//   expr :: The expressions to print
	//   str  :: The resulting string read from cin

	// gettime() -> num
	//   num :: The current Unix time

	// --- TYPES ---

	// Boolean(expr) -> bool
	//   expr :: The expression to convert to a Boolean
	//   bool :: The truthiness of expr

	// Number(expr) -> res
	//   expr :: The expression to convert to a Number
	//   res  :: The number if successful, otherwise nil

	// String(expr, ...) -> str
	//   expr :: The expressions to convert to string.
	//   str  :: The concatenated resultant string

	// { fieldlist? } ~> __thing(key, value, ...) -> thing
	//   key   :: The key of the field
	//   value :: The value to set. NOTE: key and values must come in pairs
	//   thing :: The resultant thing

	// [ expr, ... ] ~> __list(expr, ...) -> list
	//   expr :: The list of expressions to evaluate
	//   list :: The list of evaluated expressions

	// type(expr) -> str
	//   expr :: The expression to get the type of
	//   str  :: "Symbol", "String", "Number", etc...

	// --- ARITHMETIC ---

	// Helper function for infix arithmetic operations

	// x + y ~> __add(x, y) -> num
	//   x   :: The first number
	//   y   :: The second number
	//   num :: The result of x + y

	// x - y ~> __sub(x, y) -> num
	//   x   :: The first number
	//   y   :: The second number
	//   num :: The result of x - y

	// x * y ~> __mul(x, y) -> num
	//   x   :: The first number
	//   y   :: The second number
	//   num :: The result of x * y

	// x / y ~> __div(x, y) -> num
	//   x   :: The first number
	//   y   :: The second number
	//   num :: The result of x / y

	// mod(x, y) -> num
	//   x   :: The first number
	//   y   :: The second number
	//   num :: The result of x modulo y

	// -x ~> __neg(x) -> num
	//   x   :: The number to negate
	//   num :: The negation of x

	// --- COMPARISON ---

	// Helper function for infix comparison and ordering operations

	// x == y ~> __eq(x, y) -> bool
	//   x    :: The first expression
	//   y    :: The second expression
	//   bool :: Returns true if the objects are equal (by value for trivial),
	//           otherwise false.

	// x != y ~> __neq(x, y) -> bool
	//   x    :: The first expression
	//   y    :: The second expression
	//   bool :: Returns false if the objects are equal (by value for trivial),
	//           otherwise true.

	// --- LOGIC ---

	// !x ~> __not(x) -> bool
	//   x    :: The expression to evaluate
	//   bool :: The opposite truthy value of x

	// x and y ~> __and(x, y) -> bool
	//   x    :: The first expression
	//   y    :: The second expression
	//   bool :: true if x and y are truthy, otherwise false

	// x or y ~> __or(x, y) -> bool
	//   x    :: The first expression
	//   y    :: The second expression
	//   bool :: true if x or y are truthy, otherwise false

	// --- ORDERING ---

	// x > y ~> __gt(x, y) -> bool
	//   x    :: The first expression
	//   y    :: The second expression
	//   bool :: true if x > y, otherwise false

	// x >= y ~> __geq(x, y) -> bool
	//   x    :: The first expression
	//   y    :: The second expression
	//   bool :: true if x >= y, otherwise false

	// x < y ~> __lt(x, y) -> bool
	//   x    :: The first expression
	//   y    :: The second expression
	//   bool :: true if x < y, otherwise false

	// x <= y ~> __leq(x, y) -> bool
	//   x    :: The first expression
	//   y    :: The second expression
	//   bool :: true if x <= y, otherwise false

	// --- CONTROL FLOW ---

	// do expr ... end ~> __do(expr, ...) -> res
	//   expr :: The expressions to evaluate, in order
	//   res  :: The last expression, evaluated

	// if a then e ( elif b then e )* ( else e )? end ~> __if(a, e, ...) -> res
	//   a, b, c :: The conditions
	//   e       :: The expressions to evaluate
	//   res     :: The last expression evaluated

	// while cond do expr ... end ~> while(cond, expr, ...) -> res
	//   cond :: The condition of the loop
	//   expr :: The expressions to evaluate
	//   res  :: The last expression, evaluated. If none, then nil

	// --- PROCEDURES ---

	// fun [s, ...] expr ~> __fun(list, expr) -> res
	//   s    :: The symbols for the parameters
	//   list :: The list of parameters
	//   expr :: The expression representing the procedure
	//   res  :: The result of the procedure

	// return expr ~> __return(expr) -> expr
	//   expr :: The expression to return

	// --- VARIABLES ---

	// let sym = expr ~> __let(sym, expr) -> expr
	//   sym  :: The symbol to set
	//   expr :: The expr to set the symbol to

	// sym = expr ~> __assign(sym, expr) -> expr
	//   sym  :: The symbol to set
	//   expr :: The expr to set the symbol to

	// --- THING MODIFICATIONS ---

	// thing.sym   ~> __get(thing, sym) -> res
	// thing[expr] ~> __get(thing, expr) -> res
	//   thing :: The thing to get values from
	//   sym   :: The symbol to use as a key
	//   expr  :: The expression to use as a key
	//   res   :: The value if found when accessing, otherwise nil

	// thing.sym   = val ~> __set(thing, sym, val) -> val
	// thing[expr] = val ~> __set(thing, expr, val) -> val
	//   thing :: The thing to get values from
	//   sym   :: The symbol to use as a key
	//   expr  :: The expression to use as a key
	//   val   :: The value to set

	// thing:proc(args, ...) ~> __selfcall(thing, proc, args) -> res
	//   thing :: The thing to access
	//   proc  :: The procedure to call
	//   args  :: The argument to the procedure
	//   res   :: The result of the procedure

	// getmeta(thing) -> meta
	//   thing :: The thing to access
	//   meta  :: The Meta-Thing of thing if exists, nil otherwise

	// setmeta(thing, meta) -> thing
	//   thing :: The thing to set the Meta-Thing for
	//   meta  :: The metathing itself

	return &env
}

func main() {
	obmap = make(map[string]*Exp)

	special_nil = make_pexp(Special("nil"))
	special_top_scope = make_pexp(Special("top_scope"))

	env := standard_env()

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

		// fmt.Println("BEFORE TOKENIZE")

		tokens, _ := tokenize(s)

		// fmt.Println("TOKENS:")
		// for _, e := range tokens {
		// 	fmt.Print(e.kind + " " + e.lexeme + " ")
		// 	fmt.Println(e.literal)
		// }

		// fmt.Println("EXPRS:")

		exprs, err := parse(tokens)
		if err != nil {
			fmt.Println(err)
			continue
		}

		for _, e := range exprs {
			// fmt.Println((*e).as_string())

			e, rval, err := eval(env, e, special_top_scope)
			if err != nil {
				fmt.Println(err)
				panic(1)
			}

			if rval.fun != nil {
				if rval.fun == special_top_scope {
					fmt.Println("Mismatched return catch.")
					panic(1)
				}

				if repl {
					fmt.Println((*rval.val).as_string())
				}
			} else if repl {
				fmt.Println((*e).as_string())
			}
		}
	}
}
