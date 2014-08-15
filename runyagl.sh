make clean >/dev/null 2>&1
make >/dev/null 2>&1
./yaglc $1
#NAME=`echo "$1" | cut -d'.' -f1`

filename=$(basename "$1")
filename="${filename%.*}"

#>/dev/null 2>&1
g++ -v -Wall -std=c++11 example.cpp ./cpp/jsoncpp.o -o $filename -I./cpp -L./cpp     
rm -f example.cpp
./$filename
if [ "$2" == "safari" ]; then
    /Applications/Safari.app/Contents/MacOS/Safari "test.svg"
fi 

