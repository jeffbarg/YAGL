You can test out this by running these:

(Do not rely on the make file just yet, 
 just use ocamlbuild)

ocamlbuild output_test.native
#then
echo "hello()" | ./output_test.native
#or 
echo "{}" | ./output_test.native
