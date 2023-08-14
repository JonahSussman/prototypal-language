package lottypes

import (
	"fmt"
	"log"
	"strings"
)

type (
	Exp interface {
		As_string() string
		As_bool() bool
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
		Table map[TrivialExp]Exp
		Meta  Exp
	}

	Call struct {
		Name Exp
		Args List
	}

	Procedure struct {
		Params, Body List
		Env          *Env
	}

	Primitive struct {
		Fun  func(*Env, List, Exp) (Exp, RetVal, error)
		Tco  bool
		Name string
	}

	RetVal struct {
		Fun, Val Exp
	}
)

var Special_nil Special
var Special_top_scope Special

func (x *Symbol) tag_trivial()  {}
func (x *String) tag_trivial()  {}
func (x *Number) tag_trivial()  {}
func (x *Boolean) tag_trivial() {}

func (x *Number) As_string() string  { return fmt.Sprintf("%v", *x) }
func (x *String) As_string() string  { return string(*x) }
func (x *Boolean) As_string() string { return fmt.Sprintf("%v", *x) }
func (x *Symbol) As_string() string  { return fmt.Sprintf("%v", *x) }

func (x *List) As_string() string {
	var builder strings.Builder
	builder.WriteString("[")

	for i, e := range *x {
		builder.WriteString(e.As_string())
		if i != len(*x)-1 {
			builder.WriteString(", ")
		}
	}

	builder.WriteString("]")
	return builder.String()
}

func (t *Thing) As_string() string {
	var builder strings.Builder
	builder.WriteString("{ ")

	i := 0
	for k, v := range t.Table {
		builder.WriteString(k.As_string())
		builder.WriteString(" = ")
		builder.WriteString(v.As_string())
		if i < len(t.Table)-1 {
			builder.WriteString(", ")
		}
		i++
	}

	builder.WriteString(" }")
	return builder.String()
}

func (c *Call) As_string() string {
	var builder strings.Builder
	builder.WriteString(c.Name.As_string())
	builder.WriteString("(")

	for i, e := range c.Args {
		builder.WriteString(e.As_string())
		if i != len(c.Args)-1 {
			builder.WriteString(", ")
		}
	}

	builder.WriteString(")")
	return builder.String()
}

func (x *Primitive) As_string() string { return fmt.Sprintf("<primitive: %v>", &x) }
func (x *Procedure) As_string() string { return fmt.Sprintf("<procedure: %v>", &x) }
func (x *Special) As_string() string   { return fmt.Sprintf("<special: %v>", *x) }

func (x *Number) As_bool() bool { return float64(*x) != 0.0 }
func (x *String) As_bool() bool { return true }
func (x *Boolean) As_bool() bool {
	log.Printf("Boolean.As_bool = %v\n", bool(*x))
	return bool(*x)
}
func (x *Symbol) As_bool() bool    { return true }
func (x *List) As_bool() bool      { return true }
func (x *Thing) As_bool() bool     { return true }
func (x *Call) As_bool() bool      { return true }
func (x *Primitive) As_bool() bool { return true }
func (x *Procedure) As_bool() bool { return true }
func (x *Special) As_bool() bool   { return string(*x) == "nil" } // Go copies :(

func Exp_eq(l Exp, r Exp) (bool, error) {
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
				eq, err := Exp_eq((*left)[i], (*right)[i])
				if err != nil || !eq {
					return false, err
				}
			}

			return true, nil
		}
	}

	return l == r, nil
}

func Exp_neq(l Exp, r Exp) (bool, error) {
	res, err := Exp_eq(l, r)
	if err != nil {
		return false, nil
	}
	return !res, nil
}

func Exp_lt(l Exp, r Exp) (bool, error) {
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

func Exp_leq(l Exp, r Exp) (bool, error) {
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

func Exp_gt(l Exp, r Exp) (bool, error) {
	res, err := Exp_leq(l, r)
	if err != nil {
		return false, err
	}
	return !res, nil
}

func Exp_geq(l Exp, r Exp) (bool, error) {
	res, err := Exp_lt(l, r)
	if err != nil {
		return false, err
	}
	return !res, nil
}
