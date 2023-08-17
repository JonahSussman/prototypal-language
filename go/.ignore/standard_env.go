package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"reflect"
	"strconv"
)

func standard_env() *Env {
	env, _ := make_env(nil, List{}, List{})

	add := func(name string, tco bool, fun func(env *Env, args List, catch Exp) (Exp, RetVal, error)) {
		sym, _ := intern(name).(*Symbol)
		env.vars[*sym] = &Primitive{fun, tco, name}
	}

	standard_env_io(add)
	standard_env_types(add)
	standard_env_arithmetic(add)
	standard_env_comparison_and_ordering(add)
	standard_env_control_flow(add)
	standard_env_procedures(add)
	standard_env_variables(add)
	standard_env_thing_modifications(add)

	return env
}

// --- IO ---
func standard_env_io(add func(string, bool, func(*Env, List, Exp) (Exp, RetVal, error))) {
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
}

// --- TYPES ---
func standard_env_types(add func(string, bool, func(*Env, List, Exp) (Exp, RetVal, error))) {
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
}

// --- ARITHMETIC ---
func standard_env_arithmetic(add func(string, bool, func(*Env, List, Exp) (Exp, RetVal, error))) {
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
}

// --- COMPARISON AND ORDERING ---
func standard_env_comparison_and_ordering(add func(string, bool, func(*Env, List, Exp) (Exp, RetVal, error))) {
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

	/// --- COMPARSION ---

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
}

// --- CONTROL FLOW ---
func standard_env_control_flow(add func(string, bool, func(*Env, List, Exp) (Exp, RetVal, error))) {
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
}

// --- PROCEDURES ---
func standard_env_procedures(add func(string, bool, func(*Env, List, Exp) (Exp, RetVal, error))) {
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
}

// --- VARIABLES ---
func standard_env_variables(add func(string, bool, func(*Env, List, Exp) (Exp, RetVal, error))) {
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
}

// --- THING MODIFICATIONS ---
func standard_env_thing_modifications(add func(string, bool, func(*Env, List, Exp) (Exp, RetVal, error))) {
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

}
