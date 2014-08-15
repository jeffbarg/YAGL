#!/usr/bin/python

# Written by Edgar 

import subprocess
import difflib


demos_code = ["demo1.yagl", "demo2.yagl", "demo3.yagl", "demo4.yagl"]


for i in demos_code:
    subprocess.call("./runyagl.sh " + i)






