#!/usr/bin/python
import sys
from xml.dom.ext.reader.Sax2 import FromXmlStream

#load the xml file
xmlfile = open("ipc.xml", "r")
doc = FromXmlStream(file)

#open up the output ipc.c file
ipcfile = open("src/ipc.c", "w+")

