package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"reflect"
)

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
