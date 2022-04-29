#include <bits/stdc++.h>
using namespace std;

struct Exp;
struct Env;
struct Token;

template <typename T>
using Ptr = shared_ptr<T>;
using Pexp = Ptr<Exp>;
using Penv = Ptr<Env>;

class Err : public runtime_error {
public:
  Err(int line, string msg);
  Err(Token tok, string message);
};

enum ExpType {
  // "trivially comparable"
  SYMBOL, 
  STRING, NUMBER, BOOLEAN,

  // Containers
  LIST,  
  // ^ Comparable via values
  // v Comparable via references
  THING, 

  CALL,
  PRIMITIVE, PROCEDURE,
  SPECIAL,
};

struct ExpHash {
  std::size_t operator()(Exp const& e) const;
};

struct Exp {
  const ExpType type;

  union {
    string sym;
    string str;
    double num;
    bool truth;

    vector<Pexp> list;

    struct { Pexp name; vector<Pexp> args; } call;

    struct { function<Pexp(Penv, vector<Pexp>)> fun; bool tco; } prim;
    struct { vector<Pexp> params, body; Penv env; } proc;

    struct { Ptr<unordered_map<Exp, Pexp, ExpHash>> table; Pexp meta; } thing;
  };

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

  explicit operator bool() const;
};

Pexp nil = Exp::Special();

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

bool operator!=(const Exp& l, const Exp& r) {
  return !(l == r);
}

int typesize(ExpType type) {
  switch(type) {
      case SYMBOL: return sizeof(string);
      case STRING: return sizeof(string);
      case NUMBER: return sizeof(double);
      case BOOLEAN: return sizeof(bool);
      case LIST: return sizeof(vector<Pexp>);
      case CALL: return sizeof(Pexp) + sizeof(vector<Pexp>);
      case PRIMITIVE: 
        return sizeof(function<Pexp(Penv, vector<Pexp>)>) + sizeof(bool);
      case PROCEDURE:
        return 2*sizeof(vector<Pexp>) + sizeof(Penv);
      case THING:
        return sizeof(Ptr<unordered_map<Exp, Pexp, ExpHash>>);
      default: return 0;
  }
}

bool operator<(const Exp& l, const Exp& r) {
  if (l.type != r.type) return typesize(l.type) < typesize(r.type);
  switch(l.type) {
    case SYMBOL:  return l.sym < r.sym;
    case STRING:  return l.str < r.str;
    case NUMBER:  return l.num < r.num;
    case BOOLEAN: return l.truth < r.truth;
  }
  throw Err(-1, "Cannot order-compare non-well-ordered types!");
}

bool operator>(const Exp& l, const Exp& r) {
  if (l.type != r.type) return typesize(l.type) > typesize(r.type);
  switch(l.type) {
    case SYMBOL:  return l.sym > r.sym;
    case STRING:  return l.str > r.str;
    case NUMBER:  return l.num > r.num;
    case BOOLEAN: return l.truth > r.truth;
  }
  throw Err(-1, "Cannot order-compare non-well-ordered types!");
}

bool operator<=(const Exp& l, const Exp& r) {
  if (l.type != r.type) return typesize(l.type) <= typesize(r.type);
  switch(l.type) {
    case SYMBOL:  return l.sym <= r.sym;
    case STRING:  return l.str <= r.str;
    case NUMBER:  return l.num <= r.num;
    case BOOLEAN: return l.truth <= r.truth;
  }
  throw Err(-1, "Cannot order-compare non-well-ordered types!");
}

bool operator>=(const Exp& l, const Exp& r) {
  if (l.type != r.type) return typesize(l.type) >= typesize(r.type);
  switch(l.type) {
    case SYMBOL:  return l.sym >= r.sym;
    case STRING:  return l.str >= r.str;
    case NUMBER:  return l.num >= r.num;
    case BOOLEAN: return l.truth >= r.truth;
  }
  throw Err(-1, "Cannot order-compare non-well-ordered types!");
}

Exp::operator bool() const {
  switch(this->type) {
    case NUMBER:  return this->num != 0.0;
    case BOOLEAN: return this->truth;
    case SPECIAL: return this != nil.get();
  }

  return true;
}

std::size_t ExpHash::operator()(Exp const& e) const {
  switch(e.type) {
    case SYMBOL:  return std::hash<std::string>{}(e.sym);
    case STRING:  return std::hash<std::string>{}(e.str);
    case NUMBER:  return std::hash<double>{}(e.num);
    case BOOLEAN: return std::hash<bool>{}(e.truth);
  }

  throw Err(-1, "Cannot hash non-trivial types.");
}

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

  /* Constructors */
  Env(Penv outer) : outer(outer) { }
  Env(Penv outer, vector<Pexp> vars, vector<Pexp> vals) : outer(outer) {
    zip(vars, vals);
  }

  /** 
   * Zip takes a list of variables and a list of values of the same length and
   * associates them with each other.
   */
  void zip(vector<Pexp> vars, vector<Pexp> vals) {
    if (vars.size() != vals.size())
      throw Err(-1, "Env.zip: Number of args does not match params.");

    for (int i = 0; i < vars.size(); i++)
      let(vars[i], vals[i]);
  }

  /* Finds the symbol's associated value */
  Pexp get(Pexp sym) {
    if (vars.find(sym) == vars.end())
      return outer ? outer->get(sym) : nullptr;

    return vars[sym];
  }

  /* Puts the symbol and value in *this* environment */
  void let(Pexp sym, Pexp exp) { 
    vars[sym] = exp; 
  }

  /** 
   * If the symbol is found in any of the outer environments, it puts the value
   * in there. Otherwise, it puts a new variable in this environment.
   */
  void set(Pexp sym, Pexp exp) {
    auto e = this;
    while (e && e->vars.find(sym) == e->vars.end()) 
      e = e->outer.get();
    e ? e->vars[sym] = exp : vars[sym] = exp;
  }
};

/* Ostream print function. */
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
      // int sz = exp.thing.env->vars.size();
      int sz = exp.thing.table->size();
      int i = 0;
      // for (auto& p : exp.thing.env->vars) {
      for (auto& p : *(exp.thing.table)) {
        cout << p.first << " = " << *p.second;
        if (i < sz - 1) 
          cout << ", ";
        i++;
      }
      os << " }";
      break;
    }
    case PRIMITIVE: os << "<primitive: " << &exp << ">"; break;
    case PROCEDURE: os << "<procedure: " << &exp << ">"; break;
    case SPECIAL:
      if (&exp == nil.get())
        os << "<nil>";
      else
        os << "<special!>";
      break;
    default: 
      os << "<unknown!>"; 
      break;
  }

  return os;
}

enum TokType {
  LPAREN, RPAREN, LBRACK, RBRACK, LCURLY, RCURLY,
  COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

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
  const TokType type;
  string lexeme;
  int line;

  string str;
  string sym;
  double num;

  Token(TokType t, string le, int li) : type(t), lexeme(le), line(li) { }

  string to_str() {
    string s;
    if (type == STR)
      s = str;
    else if (type == SYM)
      s = sym;
    else if (type == NUM)
      s = to_string(num);
    return to_string(static_cast<std::underlying_type_t<TokType>>(type)) + "|" + lexeme + "|" + s; 
  }
};

typedef vector<Token>::iterator tokitr;

vector<Token> tokenize(string src) {
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
  static auto read_num = [&]() {
    while (isdigit(peek())) advance();
    if (peek() == '.' && isdigit(peek_next())) advance();
    while (isdigit(peek())) advance();

    auto t = new_token(NUM);
    t.num = stod(src.substr(start, curr - start));
    add(t);
  };
  static auto read_symbol = [&]() {
    while (isalpha(peek()) || isdigit(peek()) || peek() == '_') advance();

    string text = src.substr(start, curr - start);
    TokType type = SYM;
    if (keywords.find(text) != keywords.end()) type = keywords[text];
    auto t = new_token(type);
    t.sym = text;
    add(t);
  };

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

  // Actual parsing
  Pexp expr(tokitr& itr);

  vector<Pexp> parse(vector<Token> tokens) {
    vector<Pexp> exps;
    tokitr itr = tokens.begin();

    while (!at_end(itr)) {
      exps.push_back(expr(itr));
    }

    return exps;
  }

  Pexp block(tokitr& itr, vector<TokType> terminators) {
    vector<Pexp> block;
    while (multimatch(itr, terminators) < 0)
      block.push_back(expr(itr));
    itr--;
    return Exp::Call(intern("__do"), block);
  }

  Pexp do_expr(tokitr& itr) { 
    auto e = block(itr, { END });
    consume(itr, END, "Expect 'end' after do block.");
    return e;
   }

  Pexp while_expr(tokitr& itr) { 
    vector<Pexp> args = { expr(itr) };
    consume(itr, DO, "Expect 'do' after while condition.");
    args.push_back(block(itr, { END }));
    consume(itr, END, "Expect 'end' after while block.");
    return Exp::Call(intern("__while"), args);
  }

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

  Pexp let_expr(tokitr& itr) { 
    auto e = intern((consume(itr, SYM, "Expect symbol.")).sym);
    consume(itr, EQ, "Expect '=' after identifier.");
    return Exp::Call(intern("__let"), { e, expr(itr) });
  }

  Pexp return_expr(tokitr& itr) {
    return Exp::Call(intern("__return"), { expr(itr) });
  }

  Pexp thing_expr(tokitr& itr) { 
    vector<Pexp> fields;
    if (!check(itr, LCURLY)) {
      do {
        if (match(itr, LBRACK)) {
          fields.push_back(expr(itr));
          consume(itr, RBRACK, "Expect ']' after bracket field.");
        } else if (match(itr, DOT)) {
          auto e = intern(
            (consume(itr, SYM, "Expected Symbol in dot field.")).sym
          );
          fields.push_back(e);
        } else if (check(itr, RCURLY)) { // Trailing comma
          break;
        } else {
          throw Err(*itr, "Expect field.");
        }
        consume(itr, EQ, "Expect '=' after field.");
        fields.push_back(expr(itr));
      } while (match(itr, COMMA));
    }
    consume(itr, RCURLY, "Expect '}' after field list.");
    return Exp::Call(intern("thing"), fields);
   }

  Pexp list_expr(tokitr& itr) { 
    vector<Pexp> elems;
    if (!check(itr, RBRACK)) {
      do {
        elems.push_back(expr(itr));
      } while (match(itr, COMMA));
    }
    consume(itr, RBRACK, "Expect ']' after list expression.");
    return Exp::Call(intern("list"), elems);
  }

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
      } else {
        break;
      }
    }
    return e;
  }

  Pexp unary(tokitr& itr) {
    if (match(itr, BANG))  return Exp::Call(intern("_not"),  { unary(itr) });
    if (match(itr, MINUS)) return Exp::Call(intern("__neg"), { unary(itr) });
    
    return call(itr);
  }

  map<TokType, string> infix_map = {
    { SLASH, "__div" }, { STAR, "__mul" }, { MINUS, "__sub" }, { PLUS, "__add" },
    { GREATER, "__gt" }, { GREATER_EQ, "__geq" }, { LESS, "__lt" }, 
    { LESS_EQ, "__leq" }, { BANG_EQ, "__neq" }, { EQ_EQ, "__eq" }, 
    { AND, "__and" }, { OR, "__or" },
  };

  Pexp infix(tokitr& itr, function<Pexp(tokitr&)> next, vector<TokType> ts) {
    Pexp e = next(itr);
    int i = multimatch(itr, ts);
    while (i >= 0) {
      e = Exp::Call(intern(infix_map[ts[i]]), { e, next(itr) });
      i = multimatch(itr, ts);
    }

    return e;
  }

  Pexp fact(tokitr& itr) { return infix(itr,unary,{ SLASH, STAR }); }
  Pexp term(tokitr& itr) { return infix(itr,fact,{ MINUS, PLUS }); }
  Pexp comp(tokitr& itr) { return infix(itr,term,{ GREATER, GREATER_EQ, LESS, LESS_EQ }); }
  Pexp eqty(tokitr& itr) { return infix(itr,comp,{ BANG_EQ, EQ_EQ }); }
  Pexp land(tokitr& itr) { return infix(itr,eqty,{ AND }); }
  Pexp lor(tokitr& itr)  { return infix(itr,land,{ OR }); }
  
  Pexp assignment(tokitr& itr) {
    Pexp e = lor(itr);
    if (match(itr, EQ)) {
      Token equals = *prev(itr);
      Pexp value = expr(itr);
      if (e->type == SYMBOL) {
        return Exp::Call(intern("__assign"), { e, value });
      } else if (e->type == CALL && e->call.name->sym == "__get") {
        return Exp::Call(intern("__set"), { e->call.args[0], e->call.args[1], value });
      }
      throw Err(equals, "Invalid assignment target.");
    }
    return e;
  }

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

/* --- EVALUATION SECTION --- */
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

vector<Pexp> eval_list(Penv env, vector<Pexp> list) {
  for (auto& e : list) e = eval(env, e);

  return list;
}

Pexp eval(Penv env, Pexp exp) {
  while (true) {
    switch(exp->type) {
      case NUMBER:
      case STRING:
      case BOOLEAN:
      case THING:
      case PRIMITIVE:
      case PROCEDURE:
      case SPECIAL:
        return exp;
      case SYMBOL: {
        auto bind = env->get(exp);
        if (!bind) throw Err(-1, "eval: Undefined symbol '" + exp->sym + "'.");
        return bind;
      }
      case CALL: {
        auto fun = eval(env, exp->call.name);
        auto arg = exp->call.args;

        if (fun->type != PRIMITIVE && fun->type != PROCEDURE) {
          cout << "found: " << *exp << endl;
          throw Err(-1, "eval: Call must be PRIMITIVE or PROCEDURE.");
        }

        // apply
        if (fun->type == PROCEDURE) {
          try {
            auto farg = eval_list(env, arg);
            auto fenv = fun->proc.env;
            env = make_shared<Env>(fenv, fun->proc.params, farg);
            exp = eval_through_tco(env, fun->proc.body);
            if (
              exp->type == CALL && 
              exp->call.name->type == SYMBOL && 
              exp->call.name->sym == "__return"
            ) {
              return eval(env, exp->call.args[0]);
            }
          } catch (Pexp ret) {
            // cout << "returning!" << endl;
            return ret;
          }
        } else if (fun->prim.tco) { // We know the exp is a primitive
          exp = fun->prim.fun(env, arg);
        } else {
          auto ret = fun->prim.fun(env, arg);
          return ret;
        }

        break;
      }
      default:
        throw Err(-1, "eval: Unknown exp type: " + to_string(exp->type));
    }
  }
}

#define PRIM(NAME) auto NAME (Penv env, vector<Pexp> args) -> Pexp
#define ADD(FUN, TCO) env->let(intern(#FUN), Exp::Primitive(FUN, TCO))

namespace standard {
  PRIM(print) {
    for (auto& e : eval_list(env, args)) cout << *e;
    return nil;
  }

  PRIM(println) {
    for (auto& e : eval_list(env, args)) cout << *e << endl;
    return nil;
  }

  PRIM(__add) {
    if (args.size() < 1) throw Err(-1, "__add: Requires at least 2 args.");
    
    double sum = 0.0;
    for (auto& e : eval_list(env, args)) {
      if (e->type != NUMBER)
        throw Err(-1, "__add: Args must be of type Number.");
      sum += e->num;
    }

    return Exp::Number(sum);
  }

  PRIM(__sub) {
    if (args.size() < 1) throw Err(-1, "__sub: Requires at least 2 args.");
    
    bool flag = false;
    double sum = 0.0;
    for (auto& e : eval_list(env, args)) {
      if (e->type != NUMBER)
        throw Err(-1, "__sub: Args must be of type Number.");
      sum = flag ? sum - e->num : e->num;
      flag = true;
    }

    return Exp::Number(sum);
  }

  PRIM(__mul) {
    if (args.size() < 1) throw Err(-1, "__mul: Requires at least 2 args.");
    
    double sum = 1.0;
    for (auto& e : eval_list(env, args)) {
      if (e->type != NUMBER)
        throw Err(-1, "__mul: Args must be of type Number.");
      sum *= e->num;
    }

    return Exp::Number(sum);
  }

  PRIM(__div) {
    if (args.size() != 2) throw Err(-1, "__div: Requires exactly 2 args.");
    args = eval_list(env, args);
    if (args[0]->type != NUMBER || args[1]->type != NUMBER)
      throw Err(-1, "__div: Args must be of type Number.");

    return Exp::Number(args[0]->num / args[1]->num);
  }

  PRIM(__neg) {
    if (args.size() != 1) throw Err(-1, "__neg: Requires exactly 1 arg.");
    args = eval_list(env, args);
    if (args[0]->type != NUMBER) 
      throw Err(-1, "__neg: Args must be of type Number.");

    return Exp::Number(-args[0]->num);
  }

  PRIM(truth) {
    if (args.size() != 1) throw Err(-1, "truth: Requires exactly 1 arg.");
    return Exp::Boolean((bool)(*eval(env, args[0])));
  } 

  PRIM(__not) {
    if (args.size() != 1) throw Err(-1, "__not: Requires exactly 1 arg.");
    return Exp::Boolean(!(bool)(*eval(env, args[0])));
  }

  PRIM(__let) {
    if (args.size() != 2) throw Err(-1, "__let: Requires exactly 2 args");
    if (args[0]->type != SYMBOL) throw Err(-1, "__let: Requires symbol.");
    auto e = eval(env, args[1]);
    env->let(args[0], e);
    return e;
  }

  PRIM(__assign) {
    if (args.size() != 2) throw Err(-1, "__assign: Requires exactly 2 args.");
    auto e = eval(env, args[1]);
    env->let(args[0], e);
    return e;
  }

  PRIM(list) {
    return Exp::List(eval_list(env, args));
  }

  // will not work raw
  PRIM(__fun) {
    if (args.size() != 2) throw Err(-1, "__fun: Requires exactly 2 args.");
    if (args[0]->type == CALL && args[0]->call.name == intern("list")) {
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

  PRIM(thing) {
    if (args.size() % 2) throw Err(-1, "thing: Requires even number of args.");
    auto table = make_shared<unordered_map<Exp, Pexp, ExpHash>>();

    for (int i = 0; i < args.size(); i += 2) {
      if (args[i]->type == SYMBOL) args[i] = Exp::String(args[i]->sym);
      (*table)[*args[i]] = eval(env, args[i+1]);
    }

    return Exp::Thing(table, nil);
  }

  PRIM(__get) {
    if (args.size() != 2) throw Err(-1, "__get: Requires exactly 2 args");
    args = eval_list(env, args);

    auto thing = args[0];
    auto key = args[1];

    // FIXME: Work with list plz thx
    if (thing->type != THING) throw Err(-1, "__get: Only works on Things. (Accessing nil?)");

    auto table = thing->thing.table;
    auto meta = thing->thing.meta;

    if (table->find(*key) != table->end()) return (*table)[*key];
    if (meta == nil) return nil;
    if (meta->type != THING) throw Err(-1, "__get: Meta must be a thing.");
    auto metatable = meta->thing.table;
    if (metatable->find(*Exp::String("__get")) == metatable->end()) return nil;
    auto metaget = (*metatable)[*Exp::String("__get")];
    if (metaget->type != PROCEDURE)
      return metaget;

    return eval(env, Exp::Call(metaget, { thing, args[1] }));
  }

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

  PRIM(__do) {
    return eval_through_tco(env, args);
  }

  PRIM(__if) {
    if (args.size() % 2) throw Err(-1, "__if: Requires even number of args.");

    for (int i = 0; i < args.size(); i += 2)
      if (truth(env, { args[i] })->truth) return args[i+1];

    return nil;
  }


  PRIM(__while) {
    if (args.size() != 2) throw Err(-1, "__while: Requires exactly 2 args.");

    Pexp res = nil;
    while ((bool)(*eval(env, args[0])))
      res = eval(env, { args[1] });

    return res;
  }

  Pexp cmp(Penv e, vector<Pexp> a, string n, function<bool(Exp&,Exp&)> op) {
    if (a.size() != 2) throw Err(-1, n + ": Requires exactly 2 args.");
    a = eval_list(e, a);
    return Exp::Boolean(op(*(a[0]), *(a[1])));
  }

  PRIM(__eq) { return cmp(env,args,"__eq", [](Exp&a,Exp&b){return a==b;}); }
  PRIM(__neq){ return cmp(env,args,"__neq",[](Exp&a,Exp&b){return a!=b;}); }
  PRIM(__and){ return cmp(env,args,"__and",[](Exp&a,Exp&b){return (bool)a&&(bool)b;}); }
  PRIM(__or) { return cmp(env,args,"__or", [](Exp&a,Exp&b){return (bool)a||(bool)b;}); }
  PRIM(__gt) { return cmp(env,args,"__gt", [](Exp&a,Exp&b){return a>b;}); }
  PRIM(__geq){ return cmp(env,args,"__geq",[](Exp&a,Exp&b){return a>=b;}); }
  PRIM(__lt) { return cmp(env,args,"__lt", [](Exp&a,Exp&b){return a<b;}); }
  PRIM(__leq){ return cmp(env,args,"__leq",[](Exp&a,Exp&b){return a<=b;}); }

  PRIM(__return) {
    if (args.size() != 1) throw Err(-1, "__return: Requires exactly 1 args.");
    throw eval(env, args[0]);
  }

  Penv standard_env() {
    Penv env = make_shared<Env>(nullptr);

    ADD(print, false);
    ADD(println, false);
    ADD(__add, false);
    ADD(__sub, false);
    ADD(__mul, false);
    ADD(__div, false);
    ADD(__neg, false);
    ADD(truth, false);
    ADD(__not, false);
    ADD(__eq,  false);
    ADD(__neq, false);
    ADD(__let, false);
    ADD(__assign, false);
    ADD(__fun, false);
    ADD(thing, false);
    ADD(__get, false);
    ADD(__set, false);
    ADD(setmeta, false);
    ADD(__do, true);
    ADD(__if, true);
    ADD(list, false);
    ADD(__while, false);
    ADD(__or, false);
    ADD(__and, false);
    ADD(__gt, false);
    ADD(__geq, false);
    ADD(__lt, false);
    ADD(__leq, false);
    ADD(__return, false);

    return env;
  }
};

Err::Err(int line, string msg) : runtime_error(
  "[line " + ((line < 0) ? "?" : to_string(line)) + "] Error: " + msg
) { }

Err::Err(Token tok, string msg) : runtime_error(
  "[line " + to_string(tok.line) + "] Error " + 
  (tok.type == END_OF_FILE ? "at EOF" : "'" + tok.lexeme + "'") + 
  ": " + msg
) { }

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
        cout << "     " << *e << endl;
        Pexp res;
        try {
          res = eval(env, e);
        } catch (Pexp r) {
          res = r;
        }
        // if (repl) cout << *res << endl;
        cout << *res << endl;
      }
    } catch (const Err& e) {
      cout << e.what() << endl;
    }
  } while (repl);

  return 0;
}