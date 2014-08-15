
# This is the source code for the YAGL compiler. 



Use the runyagl.sh script to execute the cradle to grave process of 
compiling a .yagl program. 

#Example:
	./runyagl.sh ./demo/demo1.yagl

This will output a test.svg, which is a legal SVG image. If your .yagl program
uses "open", aka to use JSON data, your .json must be in the root directory. 


#Testing:
To run the test suite, simply invoke the python script like so:
       ./run_tests.py

Note that this assumes you have a python interpreter at /usr/bin/python and 
it is intended for python 2, but 3 should work as well. 


