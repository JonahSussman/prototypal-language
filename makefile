FILES = lot.cpp env.cpp exp.cpp token.cpp
SOURCES = $(FILES:%.cpp=src/%.cpp)

all:
	g++ -v -o lot $(SOURCES)