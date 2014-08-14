make clean >/dev/null 2>&1
make >/dev/null 2>&1
./yaglc $1
NAME=`echo "$1" | cut -d'.' -f1`

filename=$(basename "$1")
filename="${filename%.*}"
                                                      # This is incorrect 
gcc -g -v -Wall -std=gnu99 $filename".c" -o $filename #>/dev/null 2>&1
# NOTE, g++ builtin_functions.cpp -I../dist/ -L../dist/
chmod a+x $filename
./$filename

open $filename".svg"

