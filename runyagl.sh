make clean >/dev/null 2>&1
make >/dev/null 2>&1
./yaglc $1
#NAME=`echo "$1" | cut -d'.' -f1`

filename=$(basename "$1")
filename="${filename%.*}"

g++ -v -Wall -std=c++11 "example.cpp" -o $filename -I./cpp >/dev/null 2>&1
rm -f example.cpp
#open $filename".svg"

