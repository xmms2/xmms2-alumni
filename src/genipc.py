#!/usr/bin/python
import sys
from xml.dom.ext.reader.Sax2 import FromXmlStream

#load the xml file
file = open("ipc.xml")
doc = FromXmlStream(file)

