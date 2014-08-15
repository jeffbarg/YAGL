#include <iostream>
#include <fstream>
#include <string>
#include <stdlib.h>
#include "json/json.h"
using namespace std;
//The singular svg created. 
string global_svg;
string style(string c, string s){return "style=\"fill:" + c + ";stroke:" + s + "\"";}
void addRect(int width, int height, int x, int y, string color, string border_color)
{global_svg.append("<rect width=\"" + to_string(width) + "\" height=\"" + to_string(height) + "\" " + style(color, border_color) + "/>");}
void addCircle(int r, int cx, int cy, string color, string border_color)
{global_svg.append("<circle cx=\"" + to_string(cx) + "\" cy=\"" + to_string(cy) + "\" " + "r=\"" + to_string(r) + "\" " + style(color, border_color) + "/>");}
void text(string title, int x, int y, int size)
{global_svg.append("<text x=\"" + to_string(x) + "\" y=\"" + to_string(y) + "\" font-family=\"Verdana\">" + title + "</text>\n");}
void canvas(int width, int height)
{global_svg.append("<?xml version=\"1.0\"?>\n<svg width=\"" + to_string(width) + "\" height=\"" + to_string(height) + "\"  viewPort=\"0 0 "  + to_string(width) + " " + to_string(height) + "\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n");}
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
  canvas(500, 500);
  text("This is text", 200, 200, 24);
  addRect(100, 100, 0, 0, "blue", "red");
  addCircle(100, 100, 40, "red", "blue");
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

