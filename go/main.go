package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"math"
	"os"
	"reflect"
	"strconv"
	"strings"
	"unicode"
)

type (
	Exp interface {
		as_string() string
		as_bool() bool
		// get_type() string
		// eval(*Env) (Exp, RetVal, error)
	}

	TrivialExp interface {
		Exp
		tag_trivial()
	}

	Number  float64
	String  string
	Boolean bool
	Symbol  string
	List    []Exp
	Special string

	Thing struct {
		table map[TrivialExp]Exp
		meta  Exp
	}

	Call struct {
		name Exp
		args List
	}

	Procedure struct {
		params, body List
		env          *Env
	}

	Primitive struct {
		fun  func(*Env, List, Exp) (Exp, RetVal, error)
		tco  bool
		name string
	}

	RetVal struct {
		fun, val Exp
	}
)

var special_nil Special
var special_top_scope Special

func (x *Symbol) tag_trivial()  {}
func (x *String) tag_trivial()  {}
func (x *Number) tag_trivial()  {}
func (x *Boolean) tag_trivial() {}

func (x *Number) as_string() string  { return fmt.Sprintf("%v", *x) }
func (x *String) as_string() string  { return string(*x) }
func (x *Boolean) as_string() string { return fmt.Sprintf("%v", *x) }
func (x *Symbol) as_string() string  { return fmt.Sprintf("%v", *x) }

func (x *List) as_string() string {
	var builder strings.Builder
	builder.WriteString("[")

	for i, e := range *x {
		builder.WriteString(e.as_string())
		if i != len(*x)-1 {
			builder.WriteString(", ")
		}
	}

	builder.WriteString("]")
	return builder.String()
}

func (t *Thing) as_string() string {
	var builder strings.Builder
	builder.WriteString("{ ")

	i := 0
	for k, v := range t.table {
		builder.WriteString(k.as_string())
		builder.WriteString(" = ")
		builder.WriteString(v.as_string())
		if i < len(t.table)-1 {
			builder.WriteString(", ")
		}
		i++
	}

	builder.WriteString(" }")
	return builder.String()
}

func (c *Call) as_string() string {
	var builder strings.Builder
	builder.WriteString(c.name.as_string())
	builder.WriteString("(")

	for i, e := range c.args {
		builder.WriteString(e.as_string())
		if i != len(c.args)-1 {
			builder.WriteString(", ")
		}
	}

	builder.WriteString(")")
	return builder.String()
}

func (x *Primitive) as_string() string { return fmt.Sprintf("<primitive: %v>", &x) }
func (x *Procedure) as_string() string { return fmt.Sprintf("<procedure: %v>", &x) }
func (x *Special) as_string() string   { return fmt.Sprintf("<special: %v>", *x) }

func (x *Number) as_bool() bool { return float64(*x) != 0.0 }
func (x *String) as_bool() bool { return true }
func (x *Boolean) as_bool() bool {
	log.Printf("Boolean.as_bool = %v\n", bool(*x))
	return bool(*x)
}
func (x *Symbol) as_bool() bool    { return true }
func (x *List) as_bool() bool      { return true }
func (x *Thing) as_bool() bool     { return true }
func (x *Call) as_bool() bool      { return true }
func (x *Primitive) as_bool() bool { return true }
func (x *Procedure) as_bool() bool { return true }
func (x *Special) as_bool() bool   { return string(*x) == "nil" } // Go copies :(

// TODO: Implement
func exp_eq(l Exp, r Exp) (bool, error) {
	switch left := l.(type) {

	case *Symbol:
		if right, ok := r.(*Symbol); ok {
			return *left == *right, nil
		}
	case *String:
		if right, ok := r.(*String); ok {
			return *left == *right, nil
		}
	case *Number:
		if right, ok := r.(*Number); ok {
			return *left == *right, nil
		}
	case *Boolean:
		if right, ok := r.(*Boolean); ok {
			return *left == *right, nil
		}
	case *List:
		if right, ok := r.(*List); ok {
			if len(*left) != len(*right) {
				return false, nil
			}

			for i := 0; i < len(*left); i++ {
				eq, err := exp_eq((*left)[i], (*right)[i])
				if err != nil || !eq {
					return false, err
				}
			}

			return true, nil
		}
	}

	return l == r, nil
}

func exp_neq(l Exp, r Exp) (bool, error) {
	res, err := exp_eq(l, r)
	if err != nil {
		return false, nil
	}
	return !res, nil
}

func exp_lt(l Exp, r Exp) (bool, error) {
	ok := true // types are the same

	switch left := l.(type) {
	case *Symbol:
		if right, ok := r.(*Symbol); ok {
			return string(*left) < string(*right), nil
		}
	case *String:
		if right, ok := r.(*String); ok {
			return string(*left) < string(*right), nil
		}
	case *Number:
		if right, ok := r.(*Number); ok {
			return float64(*left) < float64(*right), nil
		}
	case *Boolean:
		if right, ok := r.(*Boolean); ok {
			return !bool(*left) && bool(*right), nil
		}
	}

	if !ok {
		return false, fmt.Errorf("Can only order-compare same type.")
	}

	return false, fmt.Errorf("Cannot order-compare non-well-ordered types!")
}

func exp_leq(l Exp, r Exp) (bool, error) {
	ok := true // types are the same

	switch left := l.(type) {
	case *Symbol:
		if right, ok := r.(*Symbol); ok {
			return string(*left) <= string(*right), nil
		}
	case *String:
		if right, ok := r.(*String); ok {
			return string(*left) <= string(*right), nil
		}
	case *Number:
		if right, ok := r.(*Number); ok {
			return float64(*left) <= float64(*right), nil
		}
	case *Boolean:
		if right, ok := r.(*Boolean); ok {
			return !bool(*left) || bool(*right), nil
		}
	}

	if !ok {
		return false, fmt.Errorf("Can only order-compare same type.")
	}

	return false, fmt.Errorf("Cannot order-compare non-well-ordered types!")

}

func exp_gt(l Exp, r Exp) (bool, error) {
	res, err := exp_leq(l, r)
	if err != nil {
		return false, err
	}
	return !res, nil
}

func exp_geq(l Exp, r Exp) (bool, error) {
	res, err := exp_lt(l, r)
	if err != nil {
		return false, err
	}
	return !res, nil
}

var obmap map[string]Exp

func intern(name string) Exp {
	if val, ok := obmap[name]; ok {
		return val
	}
	sym := Symbol(name)
	obmap[name] = &sym
	return &sym
}

type Env struct {
	vars  map[Symbol](Exp)
	outer *Env
}

func make_env(outer *Env, vars List, vals List) (*Env, error) {
	env := Env{}
	env.outer = outer
	env.vars = make(map[Symbol]Exp)
	err := env.zip(vars, vals)
	if err != nil {
		return nil, err
	}

	return &env, nil
}

func (env *Env) find(sym Symbol) bool {
	_, ok := env.vars[sym]
	return ok
}

func (env *Env) get(sym Symbol) (exp Exp) {
	exp, ok := env.vars[sym]

	if ok && exp != nil {
		return exp
	}

	if env.outer != nil {
		return env.outer.get(sym)
	}

	return nil
}

func (env *Env) zip(vars List, vals List) error {
	if len(vars) != len(vals) {
		return fmt.Errorf("Env.zip: Number of args does not match params.")
	}

	for i := 0; i < len(vars); i++ {
		if sym, ok := vars[i].(*Symbol); !ok {
			return fmt.Errorf("Env.zip: Can only look up symbols.")
		} else {
			env.vars[*sym] = vals[i]
		}
	}

	return nil
}

func env_set(env *Env, sym Symbol, exp Exp) error {
	// println("env_set")
	e := env

	for e != nil && !e.find(sym) {
		// println("trying to find " + string((*sym).(Symbol)) + " in " + fmt.Sprintf("%v", e))
		e = e.outer
	}

	if e == nil {
		// println("Couldn't find symbol!")
		return fmt.Errorf("Could not find symbol in env!")
	}

	// println("found symbol in " + fmt.Sprintf("%v", e))
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

func parse(tokens []token) (exps []Exp, err error) {
	var idx int
	for !parse_at_end(tokens, &idx) {
		var exp Exp
		exp, err = parse_expr(tokens, &idx)

		if err != nil {
			return nil, err
		}

		exps = append(exps, exp)
	}

	return exps, nil
}

func parse_expr(tks []token, idx *int) (ret Exp, err error) {
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

func parse_block(tks []token, idx *int, terms []string) (exp Exp, err error) {
	var block List
	for parse_multimatch(tks, idx, terms) < 0 {
		exp, err = parse_expr(tks, idx)
		if err != nil {
			return nil, err
		}
		block = append(block, exp)
	}
	(*idx)--
	return &Call{intern("__do"), block}, nil
}

func parse_do(tks []token, idx *int) (ret Exp, err error) {
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

func parse_while(tks []token, idx *int) (ret Exp, err error) {
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

	return &Call{intern("__while"), args}, nil
}

func parse_if(tks []token, idx *int) (ret Exp, err error) {
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
			arg := Boolean(true)
			args = append(args, &arg)

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
	return &Call{intern("__if"), args}, nil
}

func parse_fun(tks []token, idx *int) (ret Exp, err error) {
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

	return &Call{intern("__fun"), List{&params, e}}, nil
}

func parse_let(tks []token, idx *int) (ret Exp, err error) {
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

	return &Call{intern("__let"), List{ret, x}}, nil
}

func parse_return(tks []token, idx *int) (ret Exp, err error) {
	e, err := parse_expr(tks, idx)
	if err != nil {
		return nil, err
	}
	return &Call{intern("__return"), List{e}}, nil
}

func parse_assignment(tks []token, idx *int) (ret Exp, err error) {
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

		if _, ok := ret.(*Symbol); ok {
			return &Call{intern("__assign"), List{ret, value}}, nil
		} else if e, ok := ret.(*Call); ok && e.name == intern("__get") {
			return &Call{intern("__set"), List{e.args[0], e.args[1], value}}, nil
		}

		return nil, token_error(equals, "Invalid assignment target,")
	}

	return ret, nil
}

func parse_infix(
	tks []token,
	idx *int,
	next func([]token, *int) (ret Exp, err error),
	ts []string,
) (ret Exp, err error) {
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

		exp := Call{intern(infix_map[ts[i]]), List{ret, next_exp}}
		ret = &exp

		i = parse_multimatch(tks, idx, ts)
	}

	return ret, nil
}

func parse_lor(tks []token, idx *int) (ret Exp, err error) {
	return parse_infix(tks, idx, parse_land, []string{"OR"})
}

func parse_land(tks []token, idx *int) (ret Exp, err error) {
	return parse_infix(tks, idx, parse_eqty, []string{"AND"})
}

func parse_eqty(tks []token, idx *int) (ret Exp, err error) {
	return parse_infix(tks, idx, parse_comp, []string{"BANG_EQ", "EQ_EQ"})
}

func parse_comp(tks []token, idx *int) (ret Exp, err error) {
	return parse_infix(tks, idx, parse_term, []string{"GREATER", "GREATER_EQ", "LESS", "LESS_EQ"})
}

func parse_term(tks []token, idx *int) (ret Exp, err error) {
	return parse_infix(tks, idx, parse_fact, []string{"MINUS", "PLUS"})
}

func parse_fact(tks []token, idx *int) (ret Exp, err error) {
	return parse_infix(tks, idx, parse_unary, []string{"SLASH", "STAR"})
}

func parse_unary(tks []token, idx *int) (ret Exp, err error) {
	if parse_match(tks, idx, "BANG") {
		y, err := parse_unary(tks, idx)
		if err != nil {
			return nil, err
		}

		return &Call{intern("__not"), List{y}}, nil
	}
	if parse_match(tks, idx, "MINUS") {
		y, err := parse_unary(tks, idx)
		if err != nil {
			return nil, err
		}

		return &Call{intern("__neg"), List{y}}, nil
	}

	return parse_call(tks, idx)
}

func parse_call(tks []token, idx *int) (ret Exp, err error) {
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

			ret = &Call{ret, args}
		} else if parse_match(tks, idx, "DOT") {
			tok, err := parse_consume(tks, idx, "SYM", "Expected field after '.'")
			if err != nil {
				return nil, err
			}

			sym := String(tok.literal.(string))
			ret = &Call{intern("__get"), List{ret, &sym}}
		} else if parse_match(tks, idx, "LBRACK") {
			x, err := parse_expr(tks, idx)
			if err != nil {
				return nil, err
			}

			c := &Call{intern("__get"), List{ret, x}}
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
			str := String(tok.literal.(string))
			return &Call{intern("__selfcall"), List{ret, &str, &args}}, nil
		} else {
			break
		}
	}

	return ret, nil
}

func parse_primary(tks []token, idx *int) (ret Exp, err error) {
	if parse_match(tks, idx, "TRUE") {
		x := Boolean(true)
		return &x, nil
	}
	if parse_match(tks, idx, "FALSE") {
		x := Boolean(false)
		return &x, nil
	}
	if parse_match(tks, idx, "NIL") {
		return &special_nil, nil
	}
	if parse_match(tks, idx, "NUM") {
		if num, ok := tks[*idx-1].literal.(float64); ok {
			x := Number(num)
			return &x, nil
		}
		return nil, token_error(tks[*idx], "Mismatched literal and token values.")
	}
	if parse_match(tks, idx, "STR") {
		if str, ok := tks[*idx-1].literal.(string); ok {
			x := String(str)
			return &x, nil
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

func parse_thing(tks []token, idx *int) (ret Exp, err error) {
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
				x := Number(i)
				fields = append(fields, &x)
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

	return &Call{intern("__thing"), fields}, nil

}

func parse_list(tks []token, idx *int) (ret Exp, err error) {
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

	return &Call{intern("__list"), elems}, nil
}

// --- EVALUATION SECTION ---

func eval_through_tco(env *Env, list List, catch Exp) (Exp, RetVal, error) {
	if len(list) == 0 {
		return &special_nil, RetVal{}, nil
	}

	// fmt.Printf("[eval_through_tco] i adr before: %v\n", (*env).get(intern("i")))
	// fmt.Printf("[eval_through_tco] i val before: %v\n", float64((*(*env).get(intern("i"))).(Number)))

	for i := 0; i < len(list)-1; i++ {
		_, rval, err := eval(env, list[i], catch)
		if err != nil {
			return nil, RetVal{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}
	}

	// fmt.Printf("[eval_through_tco] i adr after*: %v\n", (*env).get(intern("i")))
	// fmt.Printf("[eval_through_tco] i val after*: %v\n", float64((*(*env).get(intern("i"))).(Number)))

	return list[len(list)-1], RetVal{}, nil
}

func eval_list(env *Env, list List, catch Exp) (List, RetVal, error) {
	// fmt.Printf("[eval_list] list before: %v\n", list.as_string())
	// println("inside eval_list")
	var ret List

	for i, _ := range list {
		// println("calling eval on " + (*list[i]).as_string())
		// fmt.Printf("i: %v\n", i)

		item, rval, err := eval(env, list[i], catch)

		if err != nil {
			return nil, RetVal{}, err
		}

		if rval.fun != nil {
			return nil, rval, nil
		}

		// list[i] = item
		ret = append(ret, item)
	}

	// fmt.Printf("[eval_list] list after: %v\n", list.as_string())

	// return list, RetVal{}, nil
	return ret, RetVal{}, nil
}

func eval(env *Env, exp Exp, upper_catch Exp) (immediate Exp, return_value RetVal, rerr error) {
	for {
		var name string
		if DEBUG {
			if t := reflect.TypeOf(exp); t.Kind() == reflect.Ptr {
				name = "*" + t.Elem().Name()
			} else {
				name = t.Name()
			}

			log.Printf("Evaluating: (%p) %v::%v\n", exp, name, exp.as_string())
			bufio.NewReader(os.Stdin).ReadString('\n')
		}

		switch value := exp.(type) {
		case *Number:
			return exp, RetVal{}, nil

		case *String:
			return exp, RetVal{}, nil

		case *Boolean:
			return exp, RetVal{}, nil

		case *Thing:
			return exp, RetVal{}, nil

		case *Primitive:
			return exp, RetVal{}, nil

		case *Procedure:
			return exp, RetVal{}, nil

		case *Special:
			return exp, RetVal{}, nil

		case *Symbol:
			bind := (*env).get(*value)
			log.Printf("  symbol ptr: %p\n", bind)
			if bind != nil {
				log.Printf("  symbol val: %v\n", bind.as_string())
			}
			log.Println()

			if bind == nil {
				// fmt.Printf("eval::Symbol erroring out\n")
				return nil, RetVal{}, fmt.Errorf("eval: Undefined symbol '%v'", value.as_string())
			}
			return bind, RetVal{}, nil

		case *Call:
			fun, rval, err := eval(env, value.name, upper_catch)
			if err != nil {
				// println("eval::Call erroring out")
				return nil, RetVal{}, err
			}
			if rval.fun != nil {
				return nil, rval, nil
			}

			arg := value.args

			// var fun_name string
			// if t := reflect.TypeOf(*exp); t.Kind() == reflect.Ptr {
			// 	name = "*" + t.Elem().Name()
			// } else {
			// 	name = t.Name()
			// }

			// println("fun is a " + name)

			switch execution := fun.(type) {
			case *Procedure:
				this_catch_value := Special(fmt.Sprintf("procedure %v", fun))
				this_catch := &this_catch_value

				farg, rval, err := eval_list(env, arg, this_catch)
				if err != nil {
					return nil, RetVal{}, err
				}
				if rval.fun != nil {
					if rval.fun == this_catch {
						return rval.val, RetVal{}, nil
					}

					return nil, rval, nil
				}

				fenv := execution.env

				new_env, err := make_env(fenv, execution.params, farg)
				if err != nil {
					return nil, RetVal{}, nil
				}

				env = new_env

				texp, rval, err := eval_through_tco(env, execution.body, this_catch)
				if err != nil {
					return nil, RetVal{}, err
				}
				if rval.fun != nil {
					if rval.fun == this_catch {
						return rval.val, RetVal{}, nil
					}

					return nil, rval, nil
				}

				exp = texp

				// TCO return handling. VERY UGLY

				if c, is_call := exp.(*Call); is_call {
					if s, is_symbol := c.name.(*Symbol); is_symbol && string(*s) == "__return" {
						e, rval, err := eval(env, c.args[0], this_catch)
						if err != nil {
							return nil, RetVal{}, err
						}
						if rval.fun != nil {
							if rval.fun == this_catch {
								return rval.val, RetVal{}, nil
							}

							return nil, rval, nil
						}

						return e, RetVal{}, nil
					}
				}
			case *Primitive:
				exp, rval, err = execution.fun(env, arg, upper_catch)

				log.Printf("  %v (%p) result ptr: %v\n", execution.name, execution, exp)
				if exp != nil {
					log.Printf("  %v (%p) result val: %v\n", execution.name, execution, exp.as_string())
				}
				log.Println()

				if err != nil {
					// println("eval::Call::Primitive erroring out")

					return nil, RetVal{}, err
				}
				if rval.fun != nil {
					return nil, rval, nil
				}

				if !execution.tco {
					return exp, RetVal{}, nil
				}
			default:
				return nil, RetVal{}, fmt.Errorf("eval: Call must be PRIMITIVE or PROCEDURE.")
			}

			continue

		case *List:
			return nil, RetVal{}, fmt.Errorf("eval: Attempted to evaluate List.")

		default:
			return nil, RetVal{}, fmt.Errorf("eval: Unknown exp type.")
		}
	}
}

func standard_env() *Env {
	env, _ := make_env(nil, List{}, List{})

	add := func(name string, tco bool, fun func(env *Env, args List, catch Exp) (Exp, RetVal, error)) {
		sym, _ := intern(name).(*Symbol)
		env.vars[*sym] = &Primitive{fun, tco, name}
	}

	// --- IO ---

	// print(expr, ...) -> nil
	//   expr :: The expressions to print

	add("print", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		list, rval, err := eval_list(env, args, catch)
		if err != nil {
			return nil, RetVal{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}
		for _, e := range list {
			fmt.Print(e.as_string())
		}

		return &special_nil, RetVal{}, nil
	})

	// println(expr, ...) -> nil
	//   expr :: The expressions to print, then a single newline

	add("println", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		list, rval, err := eval_list(env, args, catch)
		if err != nil {
			return nil, RetVal{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}
		for _, e := range list {
			fmt.Print(e.as_string())
		}
		fmt.Println()

		return &special_nil, RetVal{}, nil
	})

	// input(expr, ...) -> str
	//   expr :: The expressions to print
	//   str  :: The resulting string read from cin

	add("input", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		list, rval, err := eval_list(env, args, catch)
		if err != nil {
			return nil, RetVal{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}
		for _, e := range list {
			fmt.Print(e.as_string())
		}

		reader := bufio.NewReader(os.Stdin)
		line, err := reader.ReadString('\n')
		if err != nil {
			return nil, RetVal{}, err
		}

		line = line[:len(line)-1]
		ret := String(line)
		return &ret, RetVal{}, nil
	})

	// gettime() -> num
	//   num :: The current Unix time

	// --- TYPES ---

	// Boolean(expr) -> bool
	//   expr :: The expression to convert to a Boolean
	//   bool :: The truthiness of expr

	// Number(expr) -> res
	//   expr :: The expression to convert to a Number
	//   res  :: The number if successful, otherwise nil

	add("Number", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		if len(args) != 1 {
			return nil, RetVal{}, fmt.Errorf("Number: Requires exactly 1 arg.")
		}

		exp, rval, err := eval(env, args[0], catch)
		if err != nil {
			return nil, RetVal{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}

		switch value := exp.(type) {
		case *Number:
			return value, RetVal{}, nil
		case *Boolean:
			if bool(*value) {
				x := Number(1)
				return &x, RetVal{}, nil
			} else {
				x := Number(0)
				return &x, RetVal{}, nil
			}
		case *String:
			conv, err := strconv.Atoi(string(*value))
			if err != nil {
				return &special_nil, RetVal{}, nil
			} else {
				x := Number(conv)
				return &x, RetVal{}, nil
			}
		}

		return &special_nil, RetVal{}, nil
	})

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

	add("type", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		if len(args) != 1 {
			return nil, RetVal{}, fmt.Errorf("type: Requires exactly 2 args.")
		}

		exp, rval, err := eval(env, args[0], catch)
		if err != nil {
			return nil, RetVal{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}

		var name string
		if t := reflect.TypeOf(exp); t.Kind() == reflect.Ptr {
			name = "*" + t.Elem().Name()
		} else {
			name = t.Name()
		}

		ret := String(name)
		return &ret, RetVal{}, nil
	})

	// --- ARITHMETIC ---

	// Helper function for infix arithmetic operations
	ifx := func(env *Env, a List, c Exp, n string, op func(float64, float64) float64) (Exp, RetVal, error) {
		if len(a) != 2 {
			return nil, RetVal{}, fmt.Errorf(n + ": Requires exactly 2 args.")
		}

		a, rval, err := eval_list(env, a, c)

		if err != nil {
			return nil, RetVal{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}

		x, xOk := a[0].(*Number)
		y, yOk := a[1].(*Number)

		if !xOk || !yOk {
			return nil, RetVal{}, fmt.Errorf(n + ": Args must be of type Number.")
		}

		ret := Number(op(float64(*x), float64(*y)))
		return &ret, RetVal{}, nil
	}

	// x + y ~> __add(x, y) -> num
	//   x   :: The first number
	//   y   :: The second number
	//   num :: The result of x + y

	add("__add", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		return ifx(env, args, catch, "__add", func(a, b float64) float64 { return a + b })
	})

	// x - y ~> __sub(x, y) -> num
	//   x   :: The first number
	//   y   :: The second number
	//   num :: The result of x - y
	add("__sub", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		return ifx(env, args, catch, "__sub", func(a, b float64) float64 { return a - b })
	})

	// x * y ~> __mul(x, y) -> num
	//   x   :: The first number
	//   y   :: The second number
	//   num :: The result of x * y
	add("__mul", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		return ifx(env, args, catch, "__mul", func(a, b float64) float64 { return a * b })
	})

	// x / y ~> __div(x, y) -> num
	//   x   :: The first number
	//   y   :: The second number
	//   num :: The result of x / y
	add("__div", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		return ifx(env, args, catch, "__div", func(a, b float64) float64 { return a / b })
	})

	// mod(x, y) -> num
	//   x   :: The first number
	//   y   :: The second number
	//   num :: The result of x modulo y
	add("mod", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		return ifx(env, args, catch, "mod", math.Mod)
	})

	// -x ~> __neg(x) -> num
	//   x   :: The number to negate
	//   num :: The negation of x
	add("__neg", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		if len(args) != 1 {
			return nil, RetVal{}, fmt.Errorf("__neg: Requires exactly 1 arg.")
		}
		a, rval, err := eval_list(env, args, catch)
		if err != nil {
			return nil, RetVal{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}

		x, ok := a[0].(*Number)
		if !ok {
			return nil, RetVal{}, fmt.Errorf("__neg: Args must be of type Number.")
		}

		ret := Number(float64(-*x))
		return &ret, RetVal{}, nil
	})

	// --- COMPARISON ---

	// Helper function for infix comparison and ordering operations
	cmp := func(env *Env, a List, c Exp, n string, op func(Exp, Exp) (bool, error)) (Exp, RetVal, error) {
		// println("cmp: 0")
		if len(a) != 2 {
			return nil, RetVal{}, fmt.Errorf(n + ": Requires exactly 2 args.")
		}
		// println("cmp: 1")
		a, rval, err := eval_list(env, a, c)
		if err != nil {
			return nil, RetVal{}, err
		}
		// println("cmp: 2")
		if rval.fun != nil {
			return nil, rval, nil
		}

		// println("cmp: 3")
		res, err := op(a[0], a[1])
		if err != nil {
			fmt.Printf("%v\n", err)
			return nil, RetVal{}, err
		}

		// println("cmp: 4")

		ret := Boolean(res)
		log.Printf("%v: Returning %v (%p)\n", n, ret, &ret)
		return &ret, RetVal{}, nil
	}

	// x == y ~> __eq(x, y) -> bool
	//   x    :: The first expression
	//   y    :: The second expression
	//   bool :: Returns true if the objects are equal (by value for trivial),
	//           otherwise false.

	add("__eq", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		return cmp(env, args, catch, "__eq", exp_eq)
	})

	// x != y ~> __neq(x, y) -> bool
	//   x    :: The first expression
	//   y    :: The second expression
	//   bool :: Returns false if the objects are equal (by value for trivial),
	//           otherwise true.

	add("__neq", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		return cmp(env, args, catch, "__neq", exp_neq)
	})

	// --- LOGIC ---

	// !x ~> __not(x) -> bool
	//   x    :: The expression to evaluate
	//   bool :: The opposite truthy value of x

	add("__not", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		if len(args) != 1 {
			return nil, RetVal{}, fmt.Errorf("__not: Requires exactly 1 arg.")
		}

		e, rval, err := eval(env, args[0], catch)
		if err != nil {
			return nil, RetVal{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}

		ret := Boolean(!e.as_bool())
		return &ret, RetVal{}, nil
	})

	// x and y ~> __and(x, y) -> bool
	//   x    :: The first expression
	//   y    :: The second expression
	//   bool :: true if x and y are truthy, otherwise false

	add("__and", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		if len(args) != 2 {
			return nil, RetVal{}, fmt.Errorf("__and: Requires exactly 2 args.")
		}

		x, rval, err := eval(env, args[0], catch)
		if err != nil {
			return nil, RetVal{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}

		if !x.as_bool() {
			ret := Boolean(false)
			return &ret, RetVal{}, nil
		}

		y, rval, err := eval(env, args[1], catch)
		if err != nil {
			return nil, RetVal{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}

		ret := Boolean(y.as_bool())
		return &ret, RetVal{}, nil
	})

	// x or y ~> __or(x, y) -> bool
	//   x    :: The first expression
	//   y    :: The second expression
	//   bool :: true if x or y are truthy, otherwise false

	add("__or", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		if len(args) != 2 {
			return nil, RetVal{}, fmt.Errorf("__or: Requires exactly 2 args.")
		}

		x, rval, err := eval(env, args[0], catch)
		if err != nil {
			return nil, RetVal{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}

		if x.as_bool() {
			ret := Boolean(true)
			return &ret, RetVal{}, nil
		}

		y, rval, err := eval(env, args[1], catch)
		if err != nil {
			return nil, RetVal{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}

		ret := Boolean(y.as_bool())
		return &ret, RetVal{}, nil
	})

	// --- ORDERING ---

	// x > y ~> __gt(x, y) -> bool
	//   x    :: The first expression
	//   y    :: The second expression
	//   bool :: true if x > y, otherwise false

	add("__gt", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		return cmp(env, args, catch, "__gt", exp_gt)
	})

	// x >= y ~> __geq(x, y) -> bool
	//   x    :: The first expression
	//   y    :: The second expression
	//   bool :: true if x >= y, otherwise false

	add("__geq", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		return cmp(env, args, catch, "__geq", exp_geq)
	})

	// x < y ~> __lt(x, y) -> bool
	//   x    :: The first expression
	//   y    :: The second expression
	//   bool :: true if x < y, otherwise false

	add("__lt", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		return cmp(env, args, catch, "__lt", exp_lt)
	})

	// x <= y ~> __leq(x, y) -> bool
	//   x    :: The first expression
	//   y    :: The second expression
	//   bool :: true if x <= y, otherwise false

	add("__leq", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		return cmp(env, args, catch, "__leq", exp_leq)
	})

	// --- CONTROL FLOW ---

	// do expr ... end ~> __do(expr, ...) -> res
	//   expr :: The expressions to evaluate, in order
	//   res  :: The last expression, evaluated

	add("__do", true, eval_through_tco)

	// if a then e ( elif b then e )* ( else e )? end ~> __if(a, e, ...) -> res
	//   a, b, c :: The conditions
	//   e       :: The expressions to evaluate
	//   res     :: The last expression evaluated

	add("__if", true, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		if len(args)%2 == 1 {
			return nil, RetVal{}, fmt.Errorf("__if: Requires even number of args.")
		}

		for i := 0; i < len(args); i += 2 {
			e, rval, err := eval(env, args[i], catch)
			if err != nil {
				return nil, RetVal{}, err
			}
			if rval.fun != nil {
				return nil, rval, nil
			}
			if e.as_bool() {
				return args[i+1], RetVal{}, nil
			}
		}

		return &special_nil, RetVal{}, nil
	})

	// while cond do expr ... end ~> __while(cond, expr, ...) -> res
	//   cond :: The condition of the loop
	//   expr :: The expressions to evaluate
	//   res  :: The last expression, evaluated. If none, then nil

	add("__while", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		if len(args) != 2 {
			return nil, RetVal{}, fmt.Errorf("__while: Requires exactly 2 args.")
		}

		var res Exp = &special_nil

		for {
			// println("__while: Evaluating condition")
			// fmt.Printf("[while] env before: %v\n", env)
			// fmt.Printf("[while] i adr before: %v\n", (*env).get(intern("i")))
			// fmt.Printf("[while] i val before: %v\n", float64((*(*env).get(intern("i"))).(Number)))

			cond_exp, cond_rval, cond_err := eval(env, args[0], catch)
			if cond_err != nil {
				return nil, RetVal{}, cond_err
			}
			// println("__while: cond_err is nil")

			if cond_rval.fun != nil {
				return nil, cond_rval, nil
			}

			// println("__while: cond_rval is nil")
			log.Printf("__while: cond_exp is %v\n", cond_exp)

			if !cond_exp.as_bool() {
				log.Println("__while: condition is false")
				break
			}

			log.Println("__while: condition is true")

			// fmt.Printf("[while] i adr during: %v\n", (*env).get(intern("i")))
			// fmt.Printf("[while] i val during: %v\n", float64((*(*env).get(intern("i"))).(Number)))
			// println("While condition is true")
			// println("__while: Evaluating interior")

			res_exp, res_rval, res_err := eval(env, args[1], catch)

			// println("__while: Evaluated interior")

			if res_err != nil {
				// fmt.Println("%v", res_err)
				return nil, RetVal{}, res_err
			}

			// println("__while: err is nil")

			if res_rval.fun != nil {
				return nil, res_rval, nil
			}

			// println("__while: fun is nil")

			res = res_exp

			// println("__while: About to start next loop")
			// fmt.Printf("[while] env after: %v\n", env)
			// fmt.Printf("[while] i adr after: %v\n", (*env).get(intern("i")))
			// fmt.Printf("[while] i val after: %v\n", float64((*(*env).get(intern("i"))).(Number)))

			// reader := bufio.NewReader(os.Stdin)
			// reader.ReadString('\n')
		}

		return res, RetVal{}, nil
	})

	// --- PROCEDURES ---

	// fun [s, ...] expr ~> __fun(list, expr) -> res
	//   s    :: The symbols for the parameters
	//   list :: The list of parameters
	//   expr :: The expression representing the procedure
	//   res  :: The result of the procedure

	add("__fun", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		if len(args) != 2 {
			return nil, RetVal{}, fmt.Errorf("__fun: Requires exactly 2 arg.")
		}

		list_call, ok := args[0].(*Call)

		if ok && list_call.name == intern("__list") {
			args[0] = &(list_call.args)
		}

		parameter_list, ok := args[0].(*List)
		if !ok {
			return nil, RetVal{}, fmt.Errorf("__fun: Requires parameter list.")
		}

		for _, parameter := range *parameter_list {
			if _, ok := parameter.(*Symbol); !ok {
				return nil, RetVal{}, fmt.Errorf("__fun: Parameters must be symbols.")
			}
		}

		var body List

		do_call, ok := args[0].(*Call)
		if ok {
			if _, ok := do_call.name.(*Symbol); ok && do_call.name == intern("__do") {
				body = do_call.args
			}
		} else {
			body = List{args[1]}
		}

		return &Procedure{*parameter_list, body, env}, RetVal{}, nil
	})

	// return expr ~> __return(expr) -> expr
	//   expr :: The expression to return

	add("__return", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		if len(args) != 1 {
			return nil, RetVal{}, fmt.Errorf("__return: Requires exactly 1 arg.")
		}

		this_catch_value := Special(fmt.Sprintf("return"))
		this_catch := &this_catch_value

		exp, rval, err := eval(env, args[0], this_catch)

		if err != nil {
			return nil, RetVal{}, err
		}
		if rval.fun != nil {
			if rval.fun == this_catch {
				return nil, RetVal{catch, rval.val}, nil
			}

			// Don't know how this could ever happen tbh
			return nil, rval, nil
		}

		return nil, RetVal{catch, exp}, nil
	})

	// --- VARIABLES ---

	// let sym = expr ~> __let(sym, expr) -> expr
	//   sym  :: The symbol to set
	//   expr :: The expr to set the symbol to

	add("__let", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		if len(args) != 2 {
			return nil, RetVal{}, fmt.Errorf("__let: Requires exactly 2 args.")
		}

		sym, ok := args[0].(*Symbol)

		if !ok {
			return nil, RetVal{}, fmt.Errorf("__let: Requires symbol.")
		}

		exp, rval, err := eval(env, args[1], catch)
		if err != nil {
			// println("__let: Erroring out.")
			return nil, RetVal{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}

		env.vars[*sym] = exp
		return exp, RetVal{}, nil
	})

	// sym = expr ~> __assign(sym, expr) -> expr
	//   sym  :: The symbol to set
	//   expr :: The expr to set the symbol to

	add("__assign", false, func(env *Env, args List, catch Exp) (Exp, RetVal, error) {
		if len(args) != 2 {
			return nil, RetVal{}, fmt.Errorf("__assign: Requires exactly 2 args.")
		}

		sym, ok := args[0].(*Symbol)

		if !ok {
			return nil, RetVal{}, fmt.Errorf("__assign: Requires symbol.")
		}

		exp, rval, err := eval(env, args[1], catch)
		if err != nil {
			return nil, RetVal{}, err
		}
		if rval.fun != nil {
			return nil, rval, nil
		}

		env_set(env, *sym, exp)
		return exp, RetVal{}, nil
	})

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

	return env
}

var DEBUG bool

// var LOG *log.Logger

func main() {
	DEBUG = true
	DEBUG = false

	var log_output io.Writer

	if DEBUG {
		log_output = os.Stdout
	} else {
		log_output = io.Discard
	}

	log.SetOutput(log_output)
	log.SetFlags(0)

	obmap = make(map[string]Exp)

	special_nil = Special("nil")
	special_top_scope = Special("top_scope")

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

			log.Println(e.as_string())

			e, rval, err := eval(env, e, &special_top_scope)
			// fmt.Printf("%v\n", err)
			if err != nil {
				// fmt.Println(err)
				panic(err)
			}

			if rval.fun != nil {
				if rval.fun != &special_top_scope {
					fmt.Println("Mismatched return catch.")
					panic(1)
				}

				if repl {
					fmt.Println(rval.val.as_string())
				}
			} else if repl {
				fmt.Println(e.as_string())
			}
		}
	}
}
