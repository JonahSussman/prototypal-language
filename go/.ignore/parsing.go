package main

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
