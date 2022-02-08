CXX ?= g++
CFLAGS = -I. -Wall -Wpedantic -Werror

#DEPS =


%.o: %.cpp #$(DEPS)
	$(CXX) -c -o $@ $< $(CFLAGS)

corth_build: corth.o
	$(CXX) -o corth corth.o

