make clean >/dev/null 2>&1
make >/dev/null 2>&1
./yaglc $1
#NAME=`echo "$1" | cut -d'.' -f1`

filename=$(basename "$1")
#filename="${filename%.*}"

g++ -g -v -Wall -std=c++11 "example.cpp" -o $filename -L/cpp/json#>/dev/null 2>&1
# NOTE, g++ builtin_functions.cpp -I../dist/ -L../dist/
#chmod a+x $filename
#./$filename

#open $filename".svg"

