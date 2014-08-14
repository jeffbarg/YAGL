#include <iostream>
#include <fstream>
#include <string>
#include <stdlib.h>
#include <json/json.h>

using namespace std;

string global_svg;


string style(string c, string s){return "style=\"fill:" + c + ";stroke=" + s + "\"";}
    
void addRect(string width, string height, string x, string y, string color, string border_color)
{
  global_svg.append("<rect width=\"" + width + "\" height=\"" + height + "\" " + style(color, border_color) + "/>");
}

//Need something to open JSON object 

void _svg_init(string width, string height){global_svg.append("<svg width=\"" + width + "\" height=\"" + height + "\">\n");}

void title(string title){global_svg.append("<text x=\"0\" y=\"0\">" + title + "</text>\n");}

void _finished(){global_svg.append("</svg>");}

int main() 
{
  _svg_init("500", "500");
  title("test svg code!?");
  addRect("100", "100", "0", "0", "blue", "red");
  _finished();
  ofstream svg_file;
  svg_file.open("test.svg");
  svg_file << global_svg;
  svg_file.close();
  //  system("/Applications/Safari.app/Contents/MacOS/Safari test.svg");
  //test code
  //  cout << global_svg << endl;
  return 0;
}

