#!/usr/bin/python
import sys
from xml.dom.ext.reader.Sax2 import FromXmlStream

#dictionary mapping of types in the xml to C types
c_map = {}
c_map["none"] = "void"
c_map["int"] = "int"
c_map["string"] = "char *"

if __name__ == "__main__":

	#load the xml file
	xmlfile = open("ipc.xml", "r")
	doc = FromXmlStream(xmlfile)

	#open up the output ipc.c file
	ipcfile = open("genipc_out/ipc.c", "w+")

	nodes = doc.getElementsByTagName("ipc")
	if nodes[0].tagName == "ipc":
		print "Parsing XMMS2 IPC XML Description file version %s..." % \
		nodes[0].getAttribute("version")
	else:
		print "Error, XML files not IPC Description file."
