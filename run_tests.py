#!/usr/bin/python

# Written by Edgar 
import filecmp
import os

class DiffOfSvgs(Exception):
    pass

#Base line code
demos_code = ["./demo/demo1.yagl", "./demo/demo2.yagl", "./demo/demo3.yagl", "./demo/demo4.yagl"]

#Svgs to compare against
test_svgs = ["./tests/test_demo1.svg", "./tests/test_demo2.svg", "./tests/test_demo3.svg", "./tests/test_demo4.svg"]
t = "test.svg"

for (i, j) in zip(demos_code, test_svgs):
    os.system("./runyagl.sh " + i)
    # string_of_base_case = ''.join(list(open(j, "r")))
    # print string_of_base_case
    # string_of_test_case = ''.join(list(open(t, "r")))
    # print string_of_test_case
    if not filecmp.cmp(t, j):
        raise DiffOfSvgs("Demo: " + i + " has an issue and has a difference")
    
