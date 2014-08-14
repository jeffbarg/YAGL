#include <iostream>
#include <fstream>
#include <string>
#include <stdlib.h>
#include "json/json.h"
using namespace std;
//The singular svg created. 
string global_svg;
string style(string c, string s){return "style=\"fill:" + c + ";stroke=" + s + "\"";}
void addRect(int width, int height, int x, int y, string color, string border_color)
{global_svg.append("<rect width=\"" + to_string(width) + "\" height=\"" + to_string(height) + "\" " + style(color, border_color) + "/>");}
void title(string title, int width, int height)
{global_svg.append("<svg width=\"" + to_string(width) + "\" height=\"" + to_string(height) + "\">\n" + "<text x=\"0\" y=\"0\">" + title + "</text>\n");}
void _finished(){global_svg.append("\n</svg>");}

Json::Value openJson(string path)
{
  //Just creating node and reader on the stack. 
  Json::Value root_node;
  Json::Reader reader;
  ifstream test(path);
  //Mutated root_node, now it has data
  bool result = reader.parse(test, root_node, false);
  //Test code 
  // if (result)
  //   {
  //     for(int i = 0; i < root_node.size(); i++)
  // 	{
  // 	  string a = root_node[i].get("width", "ASCII").asString();
  // 	  cout << a;
  // 	}
  //   }
  return root_node;
}

int main() 
{
  title("test svg code!?", 500, 500);
  // addRect("100", "100", "0", "0", "blue", "red");
  _finished();
  ofstream svg_file;
  svg_file.open("test.svg");
  svg_file << global_svg;
  svg_file.close();
  //  system("/Applications/Safari.app/Contents/MacOS/Safari test.svg");
  //test code
  //  cout << global_svg << endl;
  // Json::Value foo = openJson("test.json");
  return 0;
}

