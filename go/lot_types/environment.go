package lottypes

import "fmt"

type Env struct {
	Vars  map[Symbol](Exp)
	Outer *Env
}

func Make_env(outer *Env, vars List, vals List) (*Env, error) {
	env := Env{}
	env.Outer = outer
	env.Vars = make(map[Symbol]Exp)
	err := env.zip(vars, vals)
	if err != nil {
		return nil, err
	}

	return &env, nil
}

func (env *Env) Find(sym Symbol) bool {
	_, ok := env.Vars[sym]
	return ok
}

func (env *Env) Get(sym Symbol) (exp Exp) {
	exp, ok := env.Vars[sym]

	if ok && exp != nil {
		return exp
	}

	if env.Outer != nil {
		return env.Outer.Get(sym)
	}

	return nil
}

func (env *Env) zip(Vars List, vals List) error {
	if len(Vars) != len(vals) {
		return fmt.Errorf("Env.zip: Number of args does not match params.")
	}

	for i := 0; i < len(Vars); i++ {
		if sym, ok := Vars[i].(*Symbol); !ok {
			return fmt.Errorf("Env.zip: Can only look up symbols.")
		} else {
			env.Vars[*sym] = vals[i]
		}
	}

	return nil
}

func Env_set(env *Env, sym Symbol, exp Exp) error {
	// println("env_Set")
	e := env

	for e != nil && !e.Find(sym) {
		// println("trying to Find " + string((*sym).(Symbol)) + " in " + fmt.Sprintf("%v", e))
		e = e.Outer
	}

	if e == nil {
		// println("Couldn't Find symbol!")
		return fmt.Errorf("Could not Find symbol in env!")
	}

	// println("found symbol in " + fmt.Sprintf("%v", e))
	e.Vars[sym] = exp

	return nil
}
