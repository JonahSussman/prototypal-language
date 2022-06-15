package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
)

type token struct {
	kind    string
	lexeme  string
	line    int
	literal interface{}
}

func tokenize(src string) []token {
	start, curr, line := 0, 0, 1
	var tokens []token

	// General helpers
	add := func(t token) { tokens = append(tokens, t) }
	at_end := func() bool { return curr >= len(src) }

	// peek = [&]() { return at_end() ? '\0' : src[curr]; };
	// peek_next = [&]() { return curr+1 >= src.size() ? '\0' : src[curr+1]; };
	// advance = [&]() { return src[curr++]; };
	// match = [&](char c) {
	// 	if (at_end() || src[curr] != c)
	// 		return false;
	// 	curr++;
	// 	return true;
	// };
	// auto new_token = [&](TokType t) {
	// 	return Token(t, src.substr(start, curr - start), line);
	// };

	return tokens
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

		fmt.Print(s)
	}
}
