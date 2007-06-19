#!/usr/bin/python
import sys
import string
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
	ipcfile = open("genipc_out/ipc.c", "w")
	xmmsclientfile = open("genipc_out/xmmsclient.h", "w");

	nodes = doc.getElementsByTagName("ipc")
	if nodes[0].nodeName == "ipc":
		print "Parsing XMMS2 IPC XML Description file version %s..." % \
		nodes[0].getAttribute("version")
	else:
		print "Error, XML files not IPC Description file."

	#parse all the enumerations
	print "Parsing enumerations"
	enum_list = nodes[0].getElementsByTagName("enum")

	for node in enum_list:
		#if its a child of the main(ipc) tag
		if node.parentNode == nodes[0]:
			print "%s" % node.getAttribute("name")

			xmmsclientfile.write("/* %s enum */\n" % \
					node.getAttribute("name"));
			xmmsclientfile.write("typedef enum {\n");

			props = node.getElementsByTagName("prop");
			for prop in props:
				xmmsclientfile.write("\tXMMSC_%s_%s,\n" % \
						(node.getAttribute("name").upper(),prop.getAttribute("name")))
			xmmsclientfile.write("} xmmsc_%s_t; \n" % \
					node.getAttribute("name"))


