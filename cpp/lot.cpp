#include <bits/stdc++.h>
using namespace std;

// I know this file is quite long. Therefore, I've demarcated the different
// sections of the document with section headers. Simply CTRL+F for
// "--- <SECTION NAME> ---" and you'll jump straight to it.

// --- TABLE OF CONTENTS ---
// - EXPRESSION DEFINITION
//   - EXPRESSION OPERATORS
// - ENVIRONMENT DEFINITION
// - TOKENIZATION SECTION
// - PARSING SECTION
// - EVALUATION SECTION
// - STANDARD ENVIRONMENT
//   - IO
//   - TYPES
//   - ARITHMETIC
//   - COMPARISON
//   - LOGIC
//   - ORDERING
//   - CONTROL FLOW
//   - PROCEDURES
//   - VARIABLES
//   - THING MODIFICATIONS
// - MAIN FUNCTION


// Forward declarations
struct Exp;
struct Env;
struct Token;

// Generalized throwable error
class Err : public runtime_error {
public:
  Err(int line, string msg);
  Err(Token tok, string message);
};

// Hasher class used in order to hash Exps
struct ExpHash {
  std::size_t operator()(Exp const& e) const;
};

template <typename T>      // Using smart pointers to facilitate memory mgmt
using Ptr = shared_ptr<T>;
using Pexp = Ptr<Exp>;
using Penv = Ptr<Env>;

// --- EXPRESSION DEFINITION ---

// Everything in this interpreter is an expression. This is implemented as an 
// Exp struct. This struct contains a single, anonymous union that constains all
// the different data an expression could hold. It also contains an ExpType type
// field that tells which of the data has been selected - a "tagged union".

// This way of implementation allows us to pass the expressions of various types
// in C++ methods, while also following C++'s strong typing, by simply passing a
// Pexp to the method.

// There are a couple of different things an Exp can be:
// - NUMBER :: A number (IEEE double precision float)
// - STRING :: A string
// - BOOLEAN :: A boolean
// - SYMBOL :: A symbol that can be looked up in an environment
// - LIST :: A list of Exps (for internal use mainly)
// - THING :: A Thing, which is a map of keys and values
// - CALL :: A call to a primitive or procedure with a list of args
// - PROCEDURE :: A user-defined function
// - PRIMITIVE :: A built-in function (__add, __mul, ...)
// - SPECIAL :: A special. Currently only used for nil

enum ExpType {
  SYMBOL, STRING, NUMBER, BOOLEAN, 
  LIST, THING, CALL, PRIMITIVE, PROCEDURE, SPECIAL
};

struct Exp {
  const ExpType type;

  union {
    // NUMBER
    //   - num :: The actual number
    double num;

    // STRING
    //   - str :: The actual string
    string str;

    // BOOLEAN
    //   - truth :: The actual booelan value
    bool truth;

    // SYMBOL
    //   - sym :: The string value of the symbol
    string sym;

    // LIST
    //   - list :: A vector of Pexp
    vector<Pexp> list;

    // THING
    //   - table :: An unordered map mapping Exps to Pexps
    //   - meta :: The meta-thing of this thing
    struct { Ptr<unordered_map<Exp, Pexp, ExpHash>> table; Pexp meta; } thing;

    // CALL
    //   - name :: The procedure or primitive to call
    //   - args :: The args to call it with
    struct { Pexp name; vector<Pexp> args; } call;
    
    // PROCEDURE
    //   - params :: The params of the procedure
    //   - body :: The body of the procedure to execute
    //   - env :: The environemnt of the procedure
    struct { vector<Pexp> params, body; Penv env; } proc;

    // PRIMITIVE
    //   - fun :: The C++ function to execute
    //   - tco :: Whether the primitive is tail-call-optimizable or not
    struct { function<Pexp(Penv, vector<Pexp>)> fun; bool tco; } prim;

    // SPECIAL
    //   No additional data, only the address of the actual Exp matters
  };

  // Constructor for Exp. Because we use a tagged union, C++ does not know which
  // memory is what automatically. Therefore, we need to initialize each field 
  // separately with the placement new operator.
  Exp(ExpType type) : type(type) {
    switch(type) {
      case SYMBOL:
        new(&sym) string();
        break;
      case STRING:
        new(&str) string();
        break;
      case NUMBER:
        num = 0.0;
        break;
      case BOOLEAN:
        truth = false;
        break;
      case LIST:
        new(&list) vector<Pexp>();
        break;
      case CALL:
        new(&call.name) Pexp();
        new(&call.args) vector<Pexp>();
        break;
      case PRIMITIVE:
        new(&prim.fun) function<Pexp(Penv, vector<Pexp>)>();
        prim.tco = false;
        break;
      case PROCEDURE:
        new(&proc.params) vector<Pexp>();
        new(&proc.body) vector<Pexp>();
        new(&proc.env) Penv();
        break;
      case THING:
        new(&thing.table) Ptr<unordered_map<Exp, Pexp, ExpHash>>();
        new(&thing.meta) Pexp();
        break;
      case SPECIAL:
        break;
    }
  }

  // Copy constructor
  Exp(const Exp& other) : Exp(other.type) {
    switch(type) {
      case SYMBOL:
        sym = other.sym;
        break;
      case STRING:
        str = other.str;
        break;
      case NUMBER:
        num = other.num;
        break;
      case BOOLEAN:
        truth = other.truth;
        break;
      case LIST:
        list = other.list;
        break;
      case CALL:
        call.name = other.call.name;
        call.args = other.call.args;
        break;
      case PRIMITIVE:
        prim.fun = other.prim.fun;
        prim.tco = other.prim.tco;
        break;
      case PROCEDURE:
        proc.params = other.proc.params;
        proc.body = other.proc.body;
        proc.env = other.proc.env;
        break;
      case THING:
        thing.table = other.thing.table;
        thing.meta = other.thing.meta;
        break;
      case SPECIAL:
        break;
    }
  }

  // Because we are using tagged unions, C++ *also* doesn't know which 
  // destructor to call. Therefore, we call them explicitly.
  ~Exp() {
    switch(type) {
      case SYMBOL:
        sym.~string();
        break;
      case STRING:
        str.~string();
        break;
      case NUMBER:
      case BOOLEAN:
        break;
      case LIST:
        list.~vector<Pexp>();
        break;
      case CALL:
        call.name.~Pexp();
        call.args.~vector<Pexp>();
        break;
      case PRIMITIVE:
        prim.fun.~function<Pexp(Penv, vector<Pexp>)>();
        break;
      case PROCEDURE:
        proc.params.~vector<Pexp>();
        proc.body.~vector<Pexp>();
        proc.env.~Penv();
        break;
      case THING:
        // thing.env.~Penv();
        thing.table.~Ptr<unordered_map<Exp, Pexp, ExpHash>>();
        thing.meta.~Pexp();
        break;
      case SPECIAL:
        break;
    }
  }

  // Each Exp has a certain "truthiness" assigned to it. Better to have it 
  // defined in one place, on the class rather than smattered about in the 
  // standard environment functions.
  explicit operator bool() const;

  // A bunch of static "pseudo-constructors" to return a new pointer to an Exp 
  // of a certain type.

  static Pexp Symbol(string sym) {
    auto e = make_shared<Exp>(SYMBOL);
    e->sym = sym;
    return e;
  }

  static Pexp String(string str) {
    auto e = make_shared<Exp>(STRING);
    e->str = str;
    return e;
  }

  static Pexp Number(double num) {
    auto e = make_shared<Exp>(NUMBER);
    e->num = num;
    return e;
  }

  static Pexp Boolean(bool truth) {
    auto e = make_shared<Exp>(BOOLEAN);
    e->truth = truth;
    return e;
  }

  static Pexp List(vector<Pexp> list) {
    auto e = make_shared<Exp>(LIST);
    e->list = list;
    return e;
  }

  static Pexp Call(Pexp name, vector<Pexp> args) {
    auto e = make_shared<Exp>(CALL);
    e->call.name = name;
    e->call.args = args;
    return e;
  }
  
  static Pexp Primitive(function<Pexp(Penv, vector<Pexp>)> fun, bool tco) {
    auto e = make_shared<Exp>(PRIMITIVE);
    e->prim.fun = fun;
    e->prim.tco = tco;
    return e;
  }

  static Pexp Procedure(vector<Pexp> params, vector<Pexp> body, Penv env) {
    auto e = make_shared<Exp>(PROCEDURE);
    e->proc.params = params;
    e->proc.body = body;
    e->proc.env = env;
    return e;
  }

  static Pexp Thing(Ptr<unordered_map<Exp, Pexp, ExpHash>> table, Pexp meta) {
    auto e = make_shared<Exp>(THING);
    e->thing.table = table;
    e->thing.meta = meta;
    return e;
  }

  static Pexp Special() {
    return make_shared<Exp>(SPECIAL);
  }
};

// Special constant nil, the abscense of data
Pexp nil = Exp::Special();

// --- EXPRESSION OPERATORS ---

// Equality operator. Trivial data types are compared by value. The rest are 
// compared by reference.
bool operator==(const Exp& l, const Exp& r) {
  if (l.type != r.type) return false;
  switch(l.type) {
    case SYMBOL:  return l.sym == r.sym;
    case STRING:  return l.str == r.str;
    case NUMBER:  return l.num == r.num;
    case BOOLEAN: return l.truth == r.truth;
    case LIST:
      if (l.list.size() != r.list.size()) return false;
      for (int i = 0; i < l.list.size(); i++)
        if (!(*(l.list[i]) == *(r.list[i]))) return false;
      return true;
  }
  return &l == &r;
}

// Simple non-equality operator
bool operator!=(const Exp& l, const Exp& r) { return !(l == r); }

// Tidy macro to define ordering operators
#define EXP_CMP(OP)                                                            \
  if (l.type != r.type) throw Err(-1, "Can only order-compare same type.");    \
  switch(l.type) {                                                             \
    case SYMBOL:  return OP <string>{}(l.sym, r.sym);                          \
    case STRING:  return OP <string>{}(l.str, r.str);                          \
    case NUMBER:  return OP <double>{}(l.num, r.num);                          \
    case BOOLEAN: return OP <bool>{}(l.truth, r.truth);                        \
  }                                                                            \
  throw Err(-1, "Cannot order-compare non-well-ordered types!");

bool operator< (const Exp& l, const Exp& r) { EXP_CMP(less); }
bool operator<=(const Exp& l, const Exp& r) { EXP_CMP(less_equal); }
bool operator> (const Exp& l, const Exp& r) { EXP_CMP(greater); }
bool operator>=(const Exp& l, const Exp& r) { EXP_CMP(greater_equal); }

// Booleans return their value. Numbers are false if they equal zero. Nil is 
// always false. Everything else is true.
Exp::operator bool() const {
  switch(this->type) {
    case NUMBER:  return this->num != 0.0;
    case BOOLEAN: return this->truth;
    case SPECIAL: return this != nil.get();
  }

  return true;
}

// Hash operator for the unordered_map present in Things.
size_t ExpHash::operator()(Exp const& e) const {
  switch(e.type) {
    case SYMBOL:  return hash<string>{}(e.sym);
    case STRING:  return hash<string>{}(e.str);
    case NUMBER:  return hash<double>{}(e.num);
    case BOOLEAN: return hash<bool>{}(e.truth);
  }

  throw Err(-1, "Cannot hash non-trivial types.");
}

// Ostream print function. Also used for converting an Exp of any type to a 
// String.
ostream &operator<<(ostream &os, const Exp& exp) {
  switch (exp.type) {
    case SYMBOL: os << exp.sym; break;
    case STRING: os << exp.str; break;
    case NUMBER: os << exp.num; break;
    case BOOLEAN: os << (exp.truth ? "true" : "false"); break;
    case LIST: {
      os << "[";
      for (int i = 0; i < exp.list.size(); i++) {
        os << *exp.list[i];
        if (i != exp.list.size() - 1) os << ", ";
      }
      os << "]";
      break;
    }
    case CALL: {
      os << *exp.call.name << "(";
      for (int i = 0; i < exp.call.args.size(); i++) {
        os << *exp.call.args[i];
        if (i != exp.call.args.size() - 1) os << ", ";
      }
      os << ")";
      break;
    }
    case THING: {
      os << "{ ";
      int sz = exp.thing.table->size();
      int i = 0;
      for (auto& p : *(exp.thing.table)) {
        os << p.first << " = " << *p.second;
        if (i++ < sz - 1) cout << ", ";
      }
      os << " }";
      break;
    }
    case PRIMITIVE: os << "<primitive: " << &exp << ">"; break;
    case PROCEDURE: os << "<procedure: " << &exp << ">"; break;
    case SPECIAL:   os << ((&exp == nil.get()) ? "<nil>" : "<special!>"); break;
    default:        os << "<unknown!>"; break;
  }

  return os;
}

// --- ENVIRONMENT DEFINITION ---

// Symbols are interned in a global object map so that we can compare only their
// pointers for equality
unordered_map<string, Pexp> obmap;
Pexp intern(string name) {
  if (obmap.find(name) != obmap.end())
    return obmap[name];

  auto sym = Exp::Symbol(name);
  obmap[name] = sym;
  return sym;
}

struct Env {
  unordered_map<Pexp, Pexp> vars;
  Penv outer;

  // Constructors
  Env(Penv outer) : outer(outer) { }
  Env(Penv outer, vector<Pexp> vars, vector<Pexp> vals) : outer(outer) {
    zip(vars, vals);
  }

  // Zip takes a list of variables and a list of values of the same length and
  // associates them with each other.
  void zip(vector<Pexp> vars, vector<Pexp> vals) {
    if (vars.size() != vals.size())
      throw Err(-1, "Env.zip: Number of args does not match params.");

    for (int i = 0; i < vars.size(); i++)
      let(vars[i], vals[i]);
  }

  // Finds the symbol's associated value
  Pexp get(Pexp sym) {
    if (vars.find(sym) == vars.end())
      return outer ? outer->get(sym) : nullptr;

    return vars[sym];
  }

  // Puts the symbol and value in *this* environment
  void let(Pexp sym, Pexp exp) { 
    vars[sym] = exp; 
  }

  // If the symbol is found in any of the outer environments, it puts the value
  // in there. Otherwise, throw an error.
  void set(Pexp sym, Pexp exp) {
    auto e = this;
    while (e && e->vars.find(sym) == e->vars.end()) 
      e = e->outer.get();
    if (!e) throw Err(-1, "Could not find symbol in env!");
    e->vars[sym] = exp;
  }
};

// --- TOKENIZATION SECTION ---

// A bunch of types of tokens. All should be fairly self-explanitory.
enum TokType {
  // ()[]{}
  LPAREN, RPAREN, LBRACK, RBRACK, LCURLY, RCURLY,
  COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR, COLON,

  BANG, BANG_EQ,
  EQ, EQ_EQ,
  GREATER, GREATER_EQ,
  LESS, LESS_EQ,

  SYM, STR, NUM,

  DO, END, WHILE, IF, THEN, ELIF, FUN, LET, TRUE, FALSE,
  AND, ELSE, NIL, OR, PRINT, RETURN,

  END_OF_FILE
};

struct Token {
  const TokType type; // Type of token
  string lexeme;      // The actual string in the source
  int line;           // The line of the token

  string str; // String literal
  string sym; // Symbol literal
  double num; // Number literal

  // Constructor
  Token(TokType t, string le, int li) : type(t), lexeme(le), line(li) { }
};

// Vector of tokens iterator typedef for ease of use
typedef vector<Token>::iterator tokitr;

// Given a source string, return a list of tokens
vector<Token> tokenize(string src) {
  // Keyword map
  static map<string, TokType> keywords = {
    { "and", AND },
    { "do", DO },
    { "elif", ELIF },
    { "else", ELSE },
    { "end", END },
    { "false", FALSE },
    { "fun", FUN },
    { "if", IF },
    { "let", LET },
    { "nil", NIL },
    { "or", OR },
    { "return", RETURN },
    { "then", THEN },
    { "true", TRUE },
    { "while", WHILE }
  };

  int start = 0;
  int curr = 0;
  int line = 1;

  vector<Token> tokens;

  // General helpers
  static auto add = [&](Token t) { tokens.push_back(t); };
  static auto at_end = [&]() { return curr >= src.size(); };
  static auto peek = [&]() { return at_end() ? '\0' : src[curr]; };
  static auto peek_next = [&]() { return curr+1 >= src.size() ? '\0' : src[curr+1]; };
  static auto advance = [&]() { return src[curr++]; };
  static auto match = [&](char c) { 
    if (at_end() || src[curr] != c) 
      return false; 
    curr++; 
    return true; 
  };
  auto new_token = [&](TokType t) { 
    return Token(t, src.substr(start, curr - start), line);
  };

  // Readers
  // Return the valid escape sequence character
  auto to_escape = [](char ch) {
    switch (ch) {
      case '\'': return '\'';
      case '\"': return '\"';
      case '\\': return '\\';
      case '?':  return '\?';
      case 'n':  return '\n';
      case 't':  return '\t';
    }

    return ch;
  };
  // Return a string token
  static auto read_str = [&]() {
    while (peek() != '\"' && !at_end()) {
      if (peek() == '\n') 
        line++;
      advance();
    }
    
    if (at_end()) {
      throw Err(line, "Unterminated string.");
      return;
    }
    advance();
    string s = src.substr(start + 1, curr - start - 2);

    stringstream ss{""};
    for (int i = 0; i < s.size(); i++) {
      if (s.at(i) == '\\') {
        switch(s.at(i+1)) {
          case '\'': ss << "\'"; i++; break;
          case '\"': ss << "\""; i++; break;
          case '\\': ss << "\\"; i++; break;
          case '?':  ss << "\?"; i++; break;
          case 'n':  ss << "\n"; i++; break;
          case 't':  ss << "\t"; i++; break;
          default:   ss << "\\";      break;
        }
      } else {
        ss << s.at(i);
      }
    }

    s = ss.str();
    auto t = new_token(STR);
    t.str = s;
    add(t);
  };
  // Return a number token
  static auto read_num = [&]() {
    while (isdigit(peek())) advance();
    if (peek() == '.' && isdigit(peek_next())) advance();
    while (isdigit(peek())) advance();

    auto t = new_token(NUM);
    t.num = stod(src.substr(start, curr - start));
    add(t);
  };
  // Return a symbol token
  static auto read_symbol = [&]() {
    while (isalpha(peek()) || isdigit(peek()) || peek() == '_') advance();

    string text = src.substr(start, curr - start);
    TokType type = SYM;
    if (keywords.find(text) != keywords.end()) type = keywords[text];
    auto t = new_token(type);
    t.sym = text;
    add(t);
  };

  // Main loop
  while (!at_end()) {
    start = curr;
    
    char c = advance();

    switch (c) {
      case '(': add(new_token(LPAREN)); break;
      case ')': add(new_token(RPAREN)); break;
      case '[': add(new_token(LBRACK)); break;
      case ']': add(new_token(RBRACK)); break;
      case '{': add(new_token(LCURLY)); break;
      case '}': add(new_token(RCURLY)); break;
      case ',': add(new_token(COMMA)); break;
      case '.': add(new_token(DOT)); break;
      case '-': add(new_token(MINUS)); break;
      case '+': add(new_token(PLUS)); break;
      case ':': add(new_token(COLON)); break;
      case ';': add(new_token(SEMICOLON)); break;
      case '*': add(new_token(STAR)); break;
      case '/': add(new_token(SLASH)); break;
      case '!': add(new_token(match('=') ? BANG_EQ : BANG)); break;
      case '=': add(new_token(match('=') ? EQ_EQ : EQ)); break;
      case '<': add(new_token(match('=') ? LESS_EQ : LESS)); break;
      case '>': add(new_token(match('=') ? GREATER_EQ : GREATER)); break;
      case '#': while (peek() != '\n' && !at_end()) advance(); break;
      case ' ': case '\r': case '\t': break;
      case '\n': line++; break;
      case '\"': read_str(); break;
      default:
        if (isdigit(c))
          read_num();
        else if (isalpha(c) || c == '_')
          read_symbol();
        else
          throw Err(line, "Unexpected char: '" + string(1, c) + "'");
        break;
    }

  }

  add(Token(END_OF_FILE, "", line));

  return tokens;
}

// --- PARSING SECTION ---

namespace parser {
  // Helper functions
  bool at_end(tokitr& itr) { 
    return (*itr).type == END_OF_FILE; 
  }
  bool check(tokitr& it, TokType t) { 
    return !at_end(it) && (*it).type == t; 
  }
  Token advance(tokitr& itr) { 
    if (!at_end(itr)) itr++; return *prev(itr);
  }
  Token consume(tokitr& it, TokType t, string m) { 
    return check(it, t) ? advance(it) : throw Err(*it, m);
  }
  void synchronize(tokitr& itr) {
    advance(itr);
    while (!at_end(itr)) {
      if ((*prev(itr)).type == SEMICOLON) return;

      switch ((*itr).type) {
        case DO:  case WHILE: case IF: 
        case FUN: case LET:   case PRINT:
          return;
      }
      advance(itr);
    }
  }
  bool match(tokitr& itr, TokType t) {
    if (!check(itr, t)) return false;

    advance(itr);
    return true;
  }
  int multimatch(tokitr& itr, vector<TokType> v) {
    for (int i = 0; i < v.size(); i++) {
      if (check(itr, v[i])) {
        advance(itr);
        return i;
      }
    }

    return -1;
  }

  // Main parsing section

  // Forward declaration of expr
  Pexp expr(tokitr& itr);

  // Main parsing function. Given a list of tokens, return a list of Pexp.
  vector<Pexp> parse(vector<Token> tokens) {
    vector<Pexp> exps;
    tokitr itr = tokens.begin();

    while (!at_end(itr))
      exps.push_back(expr(itr));

    return exps;
  }

  // Helper function. Continue to match expressions until one of the provided 
  // terminators is hit.
  Pexp block(tokitr& itr, vector<TokType> terminators) {
    vector<Pexp> block;
    while (multimatch(itr, terminators) < 0)
      block.push_back(expr(itr));
    itr--;
    return Exp::Call(intern("__do"), block);
  }

  // do_expr function
  Pexp do_expr(tokitr& itr) { 
    auto e = block(itr, { END });
    consume(itr, END, "Expect 'end' after do block.");
    return e;
  }

  // while_expr function
  Pexp while_expr(tokitr& itr) { 
    vector<Pexp> args = { expr(itr) };
    consume(itr, DO, "Expect 'do' after while condition.");
    args.push_back(block(itr, { END }));
    consume(itr, END, "Expect 'end' after while block.");
    return Exp::Call(intern("__while"), args);
  }

  // if_expr function
  Pexp if_expr(tokitr& itr) { 
    vector<Pexp> args = { expr(itr) };
    consume(itr, THEN, "Expect 'then' after if condition.");
    args.push_back(block(itr, { ELIF, ELSE, END }));
    int i = multimatch(itr, { ELIF, ELSE });
    while (i >= 0) {
      if (i == 0) {
        args.push_back(expr(itr));
        consume(itr, THEN, "Expect 'then' after elif condition.");
        args.push_back(block(itr, { ELIF, ELSE, END }));
      } else {
        args.push_back(Exp::Boolean(true));
        args.push_back(block(itr, { END }));
      }
      i = multimatch(itr, { ELIF, ELSE });
    }
    consume(itr, END, "Expect 'end' after if block.");
    return Exp::Call(intern("__if"), args);
  }

  // fun_expr function
  Pexp fun_expr(tokitr& itr) { 
    consume(itr, LBRACK, "Expect '[' for fun parameter list.");
    vector<Pexp> params;
    if (!check(itr, RBRACK)) {
      do {
        auto t = consume(itr, SYM, "Expect Symbol in param list.");
        params.push_back(intern(t.sym));
      } while (match(itr, COMMA));
    }
    consume(itr, RBRACK, "Expect ']' after parameter list.");
    return Exp::Call(intern("__fun"), { Exp::List(params), expr(itr) });
  }

  // let_expr function
  Pexp let_expr(tokitr& itr) { 
    auto e = intern((consume(itr, SYM, "Expect symbol.")).sym);
    consume(itr, EQ, "Expect '=' after identifier.");
    return Exp::Call(intern("__let"), { e, expr(itr) });
  }

  // return_expr function
  Pexp return_expr(tokitr& itr) {
    return Exp::Call(intern("__return"), { expr(itr) });
  }

  // thing_expr function
  Pexp thing_expr(tokitr& itr) { 
    vector<Pexp> fields;
    if (!check(itr, LCURLY)) {
      int i = 0;

      do {
        if (match(itr, LBRACK)) {
          fields.push_back(expr(itr));
          consume(itr, RBRACK, "Expect ']' after bracket field.");
          consume(itr, EQ, "Expect '=' after field.");
        } else if (match(itr, DOT)) {
          auto e = intern(
            (consume(itr, SYM, "Expected Symbol in dot field.")).sym
          );
          fields.push_back(e);
          consume(itr, EQ, "Expect '=' after field.");
        } else if (check(itr, RCURLY)) { // Trailing comma
          break;
        } else {
          // If no key is provided, it is automatically supplied one with an
          // integer key
          fields.push_back(Exp::Number(i++));
        }
        fields.push_back(expr(itr));
      } while (match(itr, COMMA));
    }
    consume(itr, RCURLY, "Expect '}' after field list.");
    return Exp::Call(intern("__thing"), fields);
  }

  // list_expr function
  Pexp list_expr(tokitr& itr) { 
    vector<Pexp> elems;
    if (!check(itr, RBRACK)) {
      do {
        elems.push_back(expr(itr));
      } while (match(itr, COMMA));
    }
    consume(itr, RBRACK, "Expect ']' after list expression.");
    return Exp::Call(intern("__list"), elems);
  }

  // primary expression function
  Pexp primary(tokitr& itr) {
    if (match(itr, TRUE))   return Exp::Boolean(true);
    if (match(itr, FALSE))  return Exp::Boolean(false);
    if (match(itr, NIL))    return nil;
    if (match(itr, NUM))    return Exp::Number((*prev(itr)).num);
    if (match(itr, STR))    return Exp::String((*prev(itr)).str);
    if (match(itr, SYM))    return intern((*prev(itr)).sym);
    if (match(itr, LCURLY)) return thing_expr(itr);
    if (match(itr, LBRACK)) return list_expr(itr);
    if (match(itr, LPAREN)) {
      Pexp e = expr(itr);
      consume(itr, RPAREN, "Expect ')' after grouping expression.");
      return e;
    }

    throw Err(*itr, "Expected expression.");
  }

  // Call exression function
  Pexp call(tokitr& itr) {
    Pexp e = primary(itr);

    while (true) {
      if (match(itr, LPAREN)) {
        vector<Pexp> args;
        if (!check(itr, RPAREN)) {
          do {
            if (args.size() >= 255) 
              throw Err(*itr, "Cannot have more than 255 args.");
            args.push_back(expr(itr));
          } while (match(itr, COMMA));
        }
        consume(itr, RPAREN, "Expected ')' after arguments.");
        e = Exp::Call(e, args);
      } else if (match(itr, DOT)) {
        Token t = consume(itr, SYM, "Expected field after '.'");
        e = Exp::Call(intern("__get"), { e, Exp::String(t.sym) });
      } else if (match(itr, LBRACK)) {
        Pexp c = Exp::Call(intern("__get"), { e, expr(itr) });
        consume(itr, RBRACK, "Expected ']' after field access.");
        e = c;
      } else if (match(itr, COLON)) {
        Token t = consume(itr, SYM, "Expected Symbol after ':'");
        consume(itr, LPAREN, "Expected '(' after selfcall.");
        vector<Pexp> args;
        if (!check(itr, RPAREN)) {
          do {
            if (args.size() >= 255) 
              throw Err(*itr, "Cannot have more than 255 args.");
            args.push_back(expr(itr));
          } while (match(itr, COMMA));
        }
        consume(itr, RPAREN, "Expected ')' after arguments.");
        e = Exp::Call(intern("__selfcall"), { e, Exp::String(t.sym), Exp::List(args) });
      } else {
        break;
      }
    }
    return e;
  }

  // Unary expression function
  Pexp unary(tokitr& itr) {
    if (match(itr, BANG))  return Exp::Call(intern("__not"), { unary(itr) });
    if (match(itr, MINUS)) return Exp::Call(intern("__neg"), { unary(itr) });
    
    return call(itr);
  }

  // Helper map that maps infix tokens to their respective primitive functions
  map<TokType, string> infix_map = {
    { SLASH, "__div" }, { STAR, "__mul" }, { MINUS, "__sub" }, { PLUS, "__add" },
    { GREATER, "__gt" }, { GREATER_EQ, "__geq" }, { LESS, "__lt" }, { LESS_EQ, "__leq" }, 
    { BANG_EQ, "__neq" }, { EQ_EQ, "__eq" }, 
    { AND, "__and" }, { OR, "__or" },
  };

  // Helper function to process infix expressions
  Pexp infix(tokitr& itr, function<Pexp(tokitr&)> next, vector<TokType> ts) {
    Pexp e = next(itr);
    int i = multimatch(itr, ts);
    while (i >= 0) {
      e = Exp::Call(intern(infix_map[ts[i]]), { e, next(itr) });
      i = multimatch(itr, ts);
    }

    return e;
  }

  // Infix expressions like factor, term, comparison, equality, etc...
  Pexp fact(tokitr& itr) { return infix(itr,unary,{ SLASH, STAR }); }
  Pexp term(tokitr& itr) { return infix(itr,fact,{ MINUS, PLUS }); }
  Pexp comp(tokitr& itr) { return infix(itr,term,{ GREATER, GREATER_EQ, LESS, LESS_EQ }); }
  Pexp eqty(tokitr& itr) { return infix(itr,comp,{ BANG_EQ, EQ_EQ }); }
  Pexp land(tokitr& itr) { return infix(itr,eqty,{ AND }); }
  Pexp lor(tokitr& itr)  { return infix(itr,land,{ OR }); }
  
  // Assignment expression
  Pexp assignment(tokitr& itr) {
    Pexp e = lor(itr);
    if (match(itr, EQ)) {
      Token equals = *prev(itr);
      Pexp value = expr(itr);
      if (e->type == SYMBOL) {
        return Exp::Call(intern("__assign"), { e, value });
      } else if (e->type == CALL && e->call.name->sym == "__get") {
        // If we're trying to assign to a key on a Thing, treat it differently
        return Exp::Call(intern("__set"), { e->call.args[0], e->call.args[1], value });
      }
      throw Err(equals, "Invalid assignment target.");
    }
    return e;
  }

  // Main expression function
  Pexp expr(tokitr& itr) {
    Pexp ret;
    if (match(itr, DO))          ret = do_expr(itr);
    else if (match(itr, WHILE))  ret = while_expr(itr);
    else if (match(itr, IF))     ret = if_expr(itr);
    else if (match(itr, FUN))    ret = fun_expr(itr);
    else if (match(itr, LET))    ret = let_expr(itr);
    else if (match(itr, RETURN)) ret = return_expr(itr);
    else ret = assignment(itr);

    match(itr, SEMICOLON);

    return ret;
  }
}

// --- EVALUATION SECTION ---

Pexp eval(Penv env, Pexp exp);

// Evaluates each element of list, returns last value
Pexp eval_through(Penv env, vector<Pexp> list) {
  Pexp result = nil;
  for (auto& e : list) result = eval(env, e);
  
  return result;
}

// Evaluates each element of list, except for the last. Returns last value 
Pexp eval_through_tco(Penv env, vector<Pexp> list) {
  if (list.size() == 0) return nil;

  for (int i = 0; i < list.size() - 1; i++) eval(env, list[i]);

  return list.back();
}

// Evaluates each element of a list. Returns that list.
vector<Pexp> eval_list(Penv env, vector<Pexp> list) {
  for (auto& e : list) e = eval(env, e);

  return list;
}

// Main evaluation function. Works like an eval-apply loop
Pexp eval(Penv env, Pexp exp) {
  while (true) {
    switch(exp->type) {
      // Self-evaluating expressions
      case NUMBER:
      case STRING:
      case BOOLEAN:
      case THING:
      case PRIMITIVE:
      case PROCEDURE:
      case SPECIAL:
        return exp;
      // We need to look up symbols in the environment
      case SYMBOL: {
        auto bind = env->get(exp);
        if (!bind) throw Err(-1, "eval: Undefined symbol '" + exp->sym + "'.");
        return bind;
      }
      // Calls apply their arguments to their body
      case CALL: {
        auto fun = eval(env, exp->call.name);
        auto arg = exp->call.args;

        if (fun->type != PRIMITIVE && fun->type != PROCEDURE) {
          cout << "found: " << *exp << endl;
          throw Err(-1, "eval: Call must be PRIMITIVE or PROCEDURE.");
        }

        // Apply
        if (fun->type == PROCEDURE) {
          // We use a try-catch block to catch any return values.
          try {
            auto farg = eval_list(env, arg);
            auto fenv = fun->proc.env;
            env = make_shared<Env>(fenv, fun->proc.params, farg);
            exp = eval_through_tco(env, fun->proc.body);
            // Because of TCO, we need to handle if the last expr is a return
            if (
              exp->type == CALL && 
              exp->call.name->type == SYMBOL && 
              exp->call.name->sym == "__return"
            ) {
              return eval(env, exp->call.args[0]);
            }
          } catch (Pexp ret) {
            return ret;
          }
        // At this point, we know the exp is a primitive
        } else if (fun->prim.tco) { 
          exp = fun->prim.fun(env, arg);
        } else {
          return fun->prim.fun(env, arg);
        }
        break;
      }
      default:
        throw Err(-1, "eval: Unknown exp type: " + to_string(exp->type));
    }
  }
}

// --- STANDARD ENVIRONMENT ---

// Tidy macros
#define PRIM(NAME) auto NAME (Penv env, vector<Pexp> args) -> Pexp
#define ADD(FUN, TCO) env->let(intern(#FUN), Exp::Primitive(FUN, TCO))

// I will use the following syntax to describe the primitives:
// - `~>`  means `transforms into`, meaning the parser will change it from the 
//         left to the right. Not every primitive needs transforming.
// - `->`  means `returns`
// - `...` means the primitive supports an arbitrary number of arguments

namespace standard {
  // --- IO ---

  // print(expr, ...) -> nil
  //   expr :: The expressions to print

  PRIM(print) {
    for (auto& e : eval_list(env, args)) cout << *e;
    return nil;
  }

  // println(expr, ...) -> nil
  //   expr :: The expressions to print, then a single newline

  PRIM(println) {
    for (auto& e : eval_list(env, args)) cout << *e; 
    cout << endl;
    return nil;
  }

  // input(expr, ...) -> str
  //   expr :: The expressions to print
  //   str  :: The resulting string read from cin

  PRIM(input) {
    for (auto& e : eval_list(env, args)) cout << *e;
    string s;
    getline(cin, s);
    return Exp::String(s);
  }

  // gettime() -> num
  //   num :: The current Unix time

  PRIM(gettime) {
    if (args.size() > 0) throw Err(-1, "input: Requires exactly 0 args.");
    return Exp::Number(std::time(nullptr));
  }

  // --- TYPES ---

  // Boolean(expr) -> bool
  //   expr :: The expression to convert to a Boolean
  //   bool :: The truthiness of expr

  PRIM(Boolean) {
    if (args.size() != 1) throw Err(-1, "Boolean: Requires exactly 1 arg.");
    return Exp::Boolean((bool)(*eval(env, args[0])));
  } 

  // Number(expr) -> res
  //   expr :: The expression to convert to a Number
  //   res  :: The number if successful, otherwise nil

  PRIM(Number) {
    if (args.size() != 1) throw Err(-1, "Number: Requires exactly 1 arg.");
    auto e = eval(env, args[0]);
    switch(e->type) {
      case NUMBER: return e;
      case BOOLEAN: return Exp::Number(e->truth ? 1 : 0);
      case STRING: 
        try { return Exp::Number(stod(e->str)); } catch (...) { return nil; }
    }

    return nil;
  }

  // String(expr, ...) -> str
  //   expr :: The expressions to convert to string.
  //   str  :: The concatenated resultant string

  PRIM(String) {
    if (args.size() < 1) throw Err(-1, "String: Requires at least 1 arg.");
    stringstream ss;
    args = eval_list(env, args);
    for (auto& e : args) ss << *e;
    return Exp::String(ss.str());
  }

  // { fieldlist? } ~> __thing(key, value, ...) -> thing
  //   key   :: The key of the field
  //   value :: The value to set. NOTE: key and values must come in pairs
  //   thing :: The resultant thing

  PRIM(__thing) {
    if (args.size() % 2) throw Err(-1, "thing: Requires even number of args.");
    auto table = make_shared<unordered_map<Exp, Pexp, ExpHash>>();

    for (int i = 0; i < args.size(); i += 2) {
      if (args[i]->type == SYMBOL) args[i] = Exp::String(args[i]->sym);
      (*table)[*args[i]] = eval(env, args[i+1]);
    }

    return Exp::Thing(table, nil);
  }

  // [ expr, ... ] ~> __list(expr, ...) -> list
  //   expr :: The list of expressions to evaluate
  //   list :: The list of evaluated expressions

  PRIM(__list) { return Exp::List(eval_list(env, args)); }

  // type(expr) -> str
  //   expr :: The expression to get the type of
  //   str  :: "Symbol", "String", "Number", etc...

  PRIM(type) {
    if (args.size() != 1) throw Err(-1, "typeof: Requires exactly 1 arg.");
    switch (eval(env, args[0])->type) {
      case SYMBOL:    return Exp::String("Symbol");
      case STRING:    return Exp::String("String");
      case NUMBER:    return Exp::String("Number");
      case BOOLEAN:   return Exp::String("Boolean");
      case LIST:      return Exp::String("List");
      case THING:     return Exp::String("Thing");
      case CALL:      return Exp::String("Call");
      case PRIMITIVE: return Exp::String("Primitive");
      case PROCEDURE: return Exp::String("Procedure");
      default:        return Exp::String("Special");
    }
  }

  // --- ARITHMETIC ---

  // Helper function for infix arithmetic operations
  Pexp ifx(Penv e,vector<Pexp> a,string n,function<double(double,double)> op) {
    if (a.size() != 2) throw Err(-1, n + ": Requires exactly 2 args.");
    a = eval_list(e, a);
    if (a[0]->type != NUMBER || a[1]->type != NUMBER)
      throw Err(-1, n + ": Args must be of type Number.");
    return Exp::Number(op(a[0]->num, a[1]->num));
  }

  // x + y ~> __add(x, y) -> num
  //   x   :: The first number
  //   y   :: The second number
  //   num :: The result of x + y

  PRIM(__add) { return ifx(env,args,"__add", plus<double>()); }

  // x - y ~> __sub(x, y) -> num
  //   x   :: The first number
  //   y   :: The second number
  //   num :: The result of x - y

  PRIM(__sub) { return ifx(env,args,"__sub", minus<double>()); }

  // x * y ~> __mul(x, y) -> num
  //   x   :: The first number
  //   y   :: The second number
  //   num :: The result of x * y

  PRIM(__mul) { return ifx(env,args,"__mul", multiplies<double>()); }

  // x / y ~> __div(x, y) -> num
  //   x   :: The first number
  //   y   :: The second number
  //   num :: The result of x / y

  PRIM(__div) { return ifx(env,args,"__div", divides<double>()); }

  // mod(x, y) -> num
  //   x   :: The first number
  //   y   :: The second number
  //   num :: The result of x modulo y

  PRIM(mod) { return ifx(env,args,"mod", [](auto x,auto y){return fmod(x,y);});}

  // -x ~> __neg(x) -> num
  //   x   :: The number to negate
  //   num :: The negation of x

  PRIM(__neg) {
    if (args.size() != 1) throw Err(-1, "__neg: Requires exactly 1 arg.");
    args = eval_list(env, args);
    if (args[0]->type != NUMBER) 
      throw Err(-1, "__neg: Args must be of type Number.");

    return Exp::Number(-args[0]->num);
  }

  // --- COMPARISON ---

  // Helper function for infix comparison and ordering operations
  Pexp cmp(Penv e, vector<Pexp> a, string n, function<bool(Exp&,Exp&)> op) {
    if (a.size() != 2) throw Err(-1, n + ": Requires exactly 2 args.");
    a = eval_list(e, a);
    return Exp::Boolean(op(*(a[0]), *(a[1])));
  }

  // x == y ~> __eq(x, y) -> bool
  //   x    :: The first expression
  //   y    :: The second expression
  //   bool :: Returns true if the objects are equal (by value for trivial), 
  //           otherwise false.

  PRIM(__eq) { return cmp(env,args,"__eq", equal_to<Exp&>()); }

  // x != y ~> __neq(x, y) -> bool
  //   x    :: The first expression
  //   y    :: The second expression
  //   bool :: Returns false if the objects are equal (by value for trivial), 
  //           otherwise true.

  PRIM(__neq){ return cmp(env,args,"__neq",not_equal_to<Exp&>()); }

  // --- LOGIC ---

  // !x ~> __not(x) -> bool
  //   x    :: The expression to evaluate
  //   bool :: The opposite truthy value of x

  PRIM(__not) {
    if (args.size() != 1) throw Err(-1, "__not: Requires exactly 1 arg.");
    return Exp::Boolean(!(bool)(*eval(env, args[0])));
  }

  // x and y ~> __and(x, y) -> bool
  //   x    :: The first expression
  //   y    :: The second expression
  //   bool :: true if x and y are truthy, otherwise false

  PRIM(__and){
    if (args.size() != 2) throw Err(-1, "__and: Requires exactly 2 args.");
    auto x = eval(env, args[0]);
    if (!(bool)(*x)) return Exp::Boolean(false); // Short circuit evaluation
    return Exp::Boolean((bool)(*eval(env, args[1])));
  }

  // x or y ~> __or(x, y) -> bool
  //   x    :: The first expression
  //   y    :: The second expression
  //   bool :: true if x or y are truthy, otherwise false

  PRIM(__or) {
    if (args.size() != 2) throw Err(-1, "__or: Requires exactly 2 args.");
    auto x = eval(env, args[0]);
    if ((bool)(*x)) return Exp::Boolean(true); // Short circuit evaluation
    return Exp::Boolean((bool)(*eval(env, args[1])));
  }

  // --- ORDERING ---

  // x > y ~> __gt(x, y) -> bool
  //   x    :: The first expression
  //   y    :: The second expression
  //   bool :: true if x > y, otherwise false

  PRIM(__gt) { return cmp(env,args,"__gt", greater<Exp&>()); }

  // x >= y ~> __geq(x, y) -> bool
  //   x    :: The first expression
  //   y    :: The second expression
  //   bool :: true if x >= y, otherwise false

  PRIM(__geq){ return cmp(env,args,"__geq",greater_equal<Exp&>()); }

  // x < y ~> __lt(x, y) -> bool
  //   x    :: The first expression
  //   y    :: The second expression
  //   bool :: true if x < y, otherwise false

  PRIM(__lt) { return cmp(env,args,"__lt", less<Exp&>()); }

  // x <= y ~> __leq(x, y) -> bool
  //   x    :: The first expression
  //   y    :: The second expression
  //   bool :: true if x <= y, otherwise false

  PRIM(__leq){ return cmp(env,args,"__leq",less_equal<Exp&>()); }

  // --- CONTROL FLOW ---

  // do expr ... end ~> __do(expr, ...) -> res
  //   expr :: The expressions to evaluate, in order
  //   res  :: The last expression, evaluated


  PRIM(__do) {
    return eval_through_tco(env, args);
  }

  // if a then e ( elif b then e )* ( else e )? end ~> __if(a, e, ...) -> res
  //   a, b, c :: The conditions
  //   e       :: The expressions to evaluate
  //   res     :: The last expression evaluated

  PRIM(__if) {
    if (args.size() % 2) throw Err(-1, "__if: Requires even number of args.");

    for (int i = 0; i < args.size(); i += 2)
      if ((bool)(*eval(env, args[i]))) return args[i+1];

    return nil;
  }

  // while cond do expr ... end ~> __while(cond, expr, ...) -> res
  //   cond :: The condition of the loop
  //   expr :: The expressions to evaluate
  //   res  :: The last expression, evaluated. If none, then nil

  PRIM(__while) {
    if (args.size() != 2) throw Err(-1, "__while: Requires exactly 2 args.");

    Pexp res = nil;
    while ((bool)(*eval(env, args[0])))
      res = eval(env, { args[1] });

    return res;
  }

  // --- PROCEDURES ---

  // fun [s, ...] expr ~> __fun(list, expr) -> res
  //   s    :: The symbols for the parameters
  //   list :: The list of parameters
  //   expr :: The expression representing the procedure
  //   res  :: The result of the procedure 

  PRIM(__fun) {
    if (args.size() != 2) throw Err(-1, "__fun: Requires exactly 2 args.");
    if (args[0]->type == CALL && args[0]->call.name == intern("__list")) {
      args[0] = Exp::List(args[0]->call.args);
      for (auto& a : args[0]->list) {
        if (a->type != SYMBOL) throw Err(-1, "__fun: Parameters must be symbols.");
      }
    }
    if (args[0]->type != LIST) throw Err(-1, "__fun: Requires parameter list.");

    vector<Pexp> body; 
    if (args[1]->type == CALL && 
      args[1]->call.name->type == SYMBOL &&
      args[1]->call.name->sym == "__do"
    ) {
      body = args[1]->call.args;
    } else {
      body = { args[1] };
    }

    return Exp::Procedure(args[0]->list, body, env);
  }

  // return expr ~> __return(expr) -> expr
  //   expr :: The expression to return

  PRIM(__return) {
    if (args.size() != 1) throw Err(-1, "__return: Requires exactly 1 args.");
    throw eval(env, args[0]);
  }

  // --- VARIABLES ---

  // let sym = expr ~> __let(sym, expr) -> expr
  //   sym  :: The symbol to set
  //   expr :: The expr to set the symbol to

  PRIM(__let) {
    if (args.size() != 2) throw Err(-1, "__let: Requires exactly 2 args");
    if (args[0]->type != SYMBOL) throw Err(-1, "__let: Requires symbol.");
    auto e = eval(env, args[1]);
    env->let(args[0], e);
    return e;
  }

  // sym = expr ~> __assign(sym, expr) -> expr
  //   sym  :: The symbol to set
  //   expr :: The expr to set the symbol to

  PRIM(__assign) {
    if (args.size() != 2) throw Err(-1, "__assign: Requires exactly 2 args.");
    if (args[0]->type != SYMBOL) throw Err(-1, "__assign: Requires symbol.");
    auto e = eval(env, args[1]);
    env->set(args[0], e);
    return e;
  }

  // --- THING MODIFICATIONS ---

  // thing.sym   ~> __get(thing, sym) -> res
  // thing[expr] ~> __get(thing, expr) -> res
  //   thing :: The thing to get values from
  //   sym   :: The symbol to use as a key
  //   expr  :: The expression to use as a key
  //   res   :: The value if found when accessing, otherwise nil
  
  PRIM(__get) {
    if (args.size() != 2) throw Err(-1, "__get: Requires exactly 2 args");
    args = eval_list(env, args);

    auto thing = args[0];
    auto key = args[1];

    if (thing->type != THING) throw Err(-1, "__get: Only works on Things. (Accessing nil?)");

    auto table = thing->thing.table;
    auto meta = thing->thing.meta;

    if (table->find(*key) != table->end()) return (*table)[*key];
    if (meta == nil) return nil;
    if (meta->type != THING) throw Err(-1, "__get: Meta must be a thing.");
    auto metatable = meta->thing.table;
    if (metatable->find(*Exp::String("__get")) == metatable->end()) return nil;
    auto metaget = (*metatable)[*Exp::String("__get")];
    if (metaget->type == THING)
      return eval(env, Exp::Call(intern("__get"), { metaget, args[1] }));
    if (metaget->type != PROCEDURE)
      return nil;

    return eval(env, Exp::Call(metaget, { thing, args[1] }));
  }

  // thing.sym   = val ~> __set(thing, sym, val) -> val
  // thing[expr] = val ~> __set(thing, expr, val) -> val
  //   thing :: The thing to get values from
  //   sym   :: The symbol to use as a key
  //   expr  :: The expression to use as a key
  //   val   :: The value to set

  PRIM(__set) {
    if (args.size() != 3) throw Err(-1, "__set: Requires exactly 3 args.");
    args = eval_list(env, args);
    auto thing = args[0];
    auto prop = args[1];
    auto value = args[2];
    if (thing->type != THING) throw Err(-1, "__set: 1st arg must be a Thing.");
    if (prop->type == SYMBOL) prop = Exp::String(prop->sym);

    if (value == nil) {
      (*(thing->thing.table)).erase(*prop);
    } else {
      (*(thing->thing.table))[*prop] = value;
    }
    return value;
  }

  // thing:proc(args, ...) ~> __selfcall(thing, proc, args) -> res
  //   thing :: The thing to access
  //   proc  :: The procedure to call
  //   args  :: The argument to the procedure
  //   res   :: The result of the procedure

  PRIM(__selfcall) {
    if (args.size() != 3) throw Err(-1, "__selfcall: Requires exactly 3 args.");
    auto a = eval(env, args[0]);
    auto b = eval(env, args[1]);
    if (args[2]->type != LIST) throw Err(-1, "__selfcall: Requires arg list.");
    vector<Pexp> c;
    c.push_back(a);
    for (auto x : args[2]->list) c.push_back(x);
    return eval(env, Exp::Call(__get(env, { a, b }), c));
  }

  // getmeta(thing) -> meta
  //   thing :: The thing to access
  //   meta  :: The Meta-Thing of thing if exists, nil otherwise

  PRIM(getmeta) {
    if (args.size() != 1) throw Err(-1, "getmeta: Requires exactly 2 args.");
    args = eval_list(env, args);
    if (args[0]->type != THING)
      throw Err(-1, "getmeta: Requires Things.");
    
    return args[0]->thing.meta;
  }
  
  // setmeta(thing, meta) -> thing
  //   thing :: The thing to set the Meta-Thing for
  //   meta  :: The metathing itself

  PRIM(setmeta) {
    if (args.size() != 2) throw Err(-1, "setmeta: Requires exactly 2 args");
    args = eval_list(env, args);
    auto a = args[0];
    auto b = args[1];
    if (a->type != THING || b->type != THING) 
      throw Err(-1, "setmeta: Requires Things.");
    
    a->thing.meta = b;
    return a;
  }

  // Returns a standard environment
  Penv standard_env() {
    Penv env = make_shared<Env>(nullptr);
    // IO
    ADD(print, false);   
    ADD(println, false);
    ADD(input, false);   
    ADD(gettime, false);
    // TYPES
    ADD(Boolean, false); 
    ADD(Number, false);
    ADD(String, false);  
    ADD(__thing, false);
    ADD(__list, false);  
    ADD(type, false);
    // ARITHMETIC
    ADD(__add, false);
    ADD(__sub, false);
    ADD(__mul, false);
    ADD(__div, false);
    ADD(mod, false);
    ADD(__neg, false);
    // COMPARISON
    ADD(__eq,  false);
    ADD(__neq, false);
    // LOGIC
    ADD(__not, false);
    ADD(__and, false);
    ADD(__or, false);
    // ORDERING
    ADD(__gt, false);
    ADD(__geq, false);
    ADD(__lt, false);
    ADD(__leq, false);
    // CONTROL FLOW
    ADD(__do, true);
    ADD(__if, true);
    ADD(__while, false);
    // PROCEDURES
    ADD(__fun, false);
    ADD(__return, false);
    // VARIABLES
    ADD(__let, false);
    ADD(__assign, false);
    // THING MODIFICATIONS
    ADD(__get, false);
    ADD(__set, false);
    ADD(__selfcall, false);
    ADD(getmeta, false);
    ADD(setmeta, false);

    return env;
  }
};

// Err definitions
Err::Err(int line, string msg) : runtime_error(
  "[line " + ((line < 0) ? "?" : to_string(line)) + "] Error: " + msg
) { }

Err::Err(Token tok, string msg) : runtime_error(
  "[line " + to_string(tok.line) + "] Error " + 
  (tok.type == END_OF_FILE ? "at EOF" : "'" + tok.lexeme + "'") + 
  ": " + msg
) { }

// --- MAIN FUNCTION ---
int main(int argc, char* argv[]) {
  if (argc > 2) {
    cout << "This is Jonah Sussman's language Lang o' Things (lot). Usage:\n\n";
    cout << "\tlot            - Launches the interactive REPL.\n";
    cout << "\tlot [filename] - Executes [filename].\n";
    cout << flush;
    return 1;
  }

  bool repl = (argc == 1);
  auto env = standard::standard_env();

  do {
    try {
      string s; 

      if (repl) {
        cout << "lot> " << flush;
        getline(cin, s); 
      } else {
        ifstream infile;
        infile.open(argv[1]);

        stringstream ss;
        ss << infile.rdbuf();
        s = ss.str();
      }

      auto tokens = tokenize(s);
      auto exprs = parser::parse(tokens);

      for (auto e : exprs) {
        if (repl) 
        cout << "     " << *e << endl;
        Pexp res;
        try {
          res = eval(env, e);
        } catch (Pexp r) {
          res = r;
        }
        if (repl) 
        cout << *res << endl;
      }
    } catch (const Err& e) {
      cout << e.what() << endl;
    }
  } while (repl);

  return 0;
}