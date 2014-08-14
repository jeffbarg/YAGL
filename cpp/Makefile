OBJS = jsoncpp.o builtin_functions.o


TEST:$(OBJS)
	g++ builtin_functions.cpp jsoncpp.cpp -o TEST -I./json

clean:
	rm -f TEST *~* *.*~* *.svg

