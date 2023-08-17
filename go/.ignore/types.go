package main

import (
	"fmt"
	"log"
	"strings"
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
