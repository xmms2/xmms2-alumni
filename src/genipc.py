#!/usr/bin/python
import sys
import string
import xml.dom.minidom
import sets

#dictionary mapping of types in the xml to C types
c_map = {}
c_map["none"] = "void"
c_map["int"] = "int"
c_map["uint"] = "unsigned int"
c_map["string"] = "char *"
c_map["enum"] = "int"

global_idnum = -1

objectlist = set()

cfile = open("genipc_out/ipc_cmds.c","w+")

def get_nextid():
    global global_idnum
    global_idnum +=1
    return global_idnum

def get_args(node):
    args = node.getElementsByTagName("arg")
    argstring = ""
    if args.length > 0:
	for arg in args:
	    type = arg.getElementsByTagName("type")
	    type = type[0].childNodes[1].nodeName
	    name = arg.getAttribute("name")
	    argstring = argstring + ", " + c_map[type] + " " + name

    return argstring

def write_cmd_code(node):
    cfile.write("typedef struct xmms_%s_St xmms_%s_t;\n" % \
	    (node.getAttribute("name"),node.getAttribute("name")))
    cfile.write("#include \"xmms/xmms_object.h\"\n")
    cfile.write("#include \"xmmspriv/xmms_%s_cmds.h\"\n" % node.getAttribute("name"))
    cfile.write("#include \"xmmsclient/xmmsclient.h\"\n\n")

    #init function
    cfile.write("xmms_%s_cmds_t *\nxmms_%s_cmds_init (xmms_object_t *obj)\n{\n" % \
	    (node.getAttribute("name"),node.getAttribute("name")))

    cfile.write("\txmms_%s_cmds_t *cmd = calloc (sizeof (xmms_%s_cmds_t),1);\n"% \
	    (node.getAttribute("name"),node.getAttribute("name")))
    cfile.write("\tcmd->obj = obj;\n\n")
    cfile.write("\treturn cmd;\n")

    cfile.write("}\n\n");

    #registration function
    cfile.write("void\nxmms_%s_cmds_register (xmms_ipc_t *ipc, xmms_%s_cmds_t *cmds)\n{\n" % \
	    (node.getAttribute("name"),node.getAttribute("name")))

#    cfile.write("\t
    #go through each method in the node and call xmms_ipc_object_cmd_add on them
    cfile.write("}\n\n")

def do_enums(enums):
    for node in enums:
        #if its a child of the main(ipc) tag
        if node.parentNode == nodes[0]:
            sys.stdout.write("%s..." % node.getAttribute("name"))

            xmmsclientfile.write("/* %s enum */\n" % \
                    node.getAttribute("name"));
            xmmsclientfile.write("typedef enum {\n")

            props = node.getElementsByTagName("prop")
            for prop in props:
                xmmsclientfile.write("\tXMMSC_%s_%s = %d,\n" % \
                                     (node.getAttribute("name").upper(),
                                     prop.getAttribute("name"),get_nextid()))

	    xmmsclientfile.write("\tXMMSC_%s_END" % \
		    node.getAttribute("name").upper())
            xmmsclientfile.write("\n} xmmsc_%s_t; \n\n" % \
                    node.getAttribute("name"))
            print "done"

def do_objects(objects):
    for node in objects:
        #if its a child of the main(ipc) tag
        if node.parentNode == nodes[0]:
            sys.stdout.write("%s..." % node.getAttribute("name"))

            if node.getAttribute("type") == "client" or \
	    node.getAttribute("type") == "both":
                xmmsclientfile.write("\n/* %s object properties and methods */\n" % \
                        node.getAttribute("name"))

		objectlist.add(node.getAttribute("name"))

                #enums (properties/variables)
                xmmsclientfile.write("typedef enum {\n")

                props = node.getElementsByTagName("prop")
                for prop in props:
                    type = node.getElementsByTagName("type")
#                   xmmsclientfile.write("\t%s %s;\n" % \
#                                        (c_map[type[0].childNodes[1].nodeName],
#                                        prop.getAttribute("name")))

                    xmmsclientfile.write("\tXMMSC_%s_PROPERTY_%s = %d,\n" % \
                                         (node.getAttribute("name").upper(),
                                         prop.getAttribute("name").upper(),get_nextid()))

		xmmsclientfile.write("\tXMMSC_%s_PROPERTY_END\n" % \
			node.getAttribute("name").upper())
                xmmsclientfile.write("} xmmsc_%s_properties_t;\n\n" % \
                        node.getAttribute("name"))

		#enums (methods)
		xmmsclientfile.write("typedef enum {\n");

		methods = node.getElementsByTagName("method")
		for method in methods:
		    type = node.getElementsByTagName("type")
		    xmmsclientfile.write("\tXMMSC_%s_METHOD_%s = %d,\n" % \
					 (node.getAttribute("name").upper(),
					 method.getAttribute("name").upper(),
					 get_nextid()))

		xmmsclientfile.write("\tXMMSC_%s_METHOD_END\n" % \
			node.getAttribute("name").upper())
		xmmsclientfile.write("} xmmsc_%s_methods_t;\n\n" % \
			node.getAttribute("name"))


                #higher-level method declarations
                methods = node.getElementsByTagName("method")
                for method in methods:
                    retval = method.getElementsByTagName("retval")
                    xmmsclientfile.write("%s " % \
                        (c_map[retval[0].childNodes[1].childNodes[1].nodeName]))

                    #figure out how to write the arguments
                    argstring = "xmmsc_connection_t *c"
		    argstring = argstring + get_args(method)
                    argstring = argstring + ", xmmsc_error_t *err"

                    #actually output the rest of the line
                    xmmsclientfile.write("xmmsc_%s_%s (%s);\n" % \
                                         (node.getAttribute("name"),method.getAttribute("name"),
                                        argstring))

	    if node.getAttribute("type") == "server" or \
	    node.getAttribute("type") == "both":
                #Open up the output header file
		hfile = open("genipc_out/xmms_%s_cmds.h" % \
			node.getAttribute("name"),"w+");

		objectlist.add(node.getAttribute("name"))

		#header guard
		hfile.write("#ifndef __XMMS_%s_CMD_H__\n" % \
			node.getAttribute("name"))
		hfile.write("#define __XMMS_%s_CMD_H__\n\n" % \
			node.getAttribute("name"))

		hfile.write("/* %s commands structure */\n" % node.getAttribute("name"))
		hfile.write("typedef struct {\n")

		#make function ptrs for getting/setting properties
		props = node.getElementsByTagName("prop")
		for prop in props:
		    readable = prop.getElementsByTagName("readable")
		    if readable.length > 0:
			#output getter
			rettype = prop.getElementsByTagName("type")
			rettype = rettype[0].childNodes[1].nodeName

			hfile.write("\t%s " % c_map[rettype])

			#figure out how to write the arguments
			argstring = "xmms_%s_t *obj" % node.getAttribute("name")
			argstring = argstring + get_args(prop)
			argstring = argstring + ", xmms_error_t *err"

			hfile.write("(*%s_get_%s) (%s);\n" % \
			    (node.getAttribute("name"),
				prop.getAttribute("name"),argstring))

			#output setter
			selftype = prop.getElementsByTagName("type")
			selftype = selftype[0].childNodes[1].nodeName

			hfile.write("\tvoid ")

			#figure out how to write the arguments
			argstring = "xmms_%s_t *obj" % node.getAttribute("name")
			argstring = argstring + get_args(prop)

			argstring = argstring + ", %s %s" % \
				(c_map[selftype],prop.getAttribute("name"))
			argstring = argstring + ", xmms_error_t *err"

			hfile.write("(*%s_set_%s) (%s);\n" % \
			    (node.getAttribute("name"),
				prop.getAttribute("name"),argstring))


		#make function ptrs for the methods
		methods = node.getElementsByTagName("method")
		for method in methods:
		    retval = method.getElementsByTagName("retval")
		    hfile.write("\t%s " % \
			    c_map[retval[0].childNodes[1].childNodes[1].nodeName])

		    #figure out how to write the arguments
                    argstring = "xmms_%s_t *obj" % node.getAttribute("name")
		    argstring = argstring + get_args(method)
                    argstring = argstring + ", xmms_error_t *err"

		    #actually output the rest of the line
                    hfile.write("(*%s_%s) (%s);\n" % \
                                         (node.getAttribute("name"),method.getAttribute("name"),
                                        argstring))

		hfile.write("\txmms_object_t *obj;\n")
		hfile.write("} xmms_%s_cmds_t;\n\n" % node.getAttribute("name"))

		#function declarations
		hfile.write("xmms_%s_cmds_t * xmms_%s_cmds_init (xmms_object_t *obj);\n" % \
			(node.getAttribute("name"), node.getAttribute("name")))

		hfile.write("void xmms_%s_cmds_register (xmms_ipc_t *ipc, xmms_%s_cmds_t *cmds);\n" % \
			(node.getAttribute("name"), node.getAttribute("name")))

		hfile.write("#endif\n")
		hfile.close()

		#output the code for the cmd helper functions
		write_cmd_code(node)

            print "done"

    #TODO we also need to output this somewhere the server can see it
    #output enum of all objects
    xmmsclientfile.write("\n/* Enum of all valid objects */\n")
    xmmsclientfile.write("typedef enum {\n")

    for object in objectlist:
	xmmsclientfile.write("\tXMMSC_IPC_OBJECT_%s,\n" % object.upper())

    xmmsclientfile.write("\tXMMSC_IPC_OBJECT_END\n} xmmsc_ipc_object_t;\n")


if __name__ == "__main__":

    #load the xml file
    doc = xml.dom.minidom.parse("ipc.xml")

    xmmsclientfile = open("genipc_out/xmmsclient.h","w+")

    #header guard and include file
    xmmsclientfile.write("#ifndef __GEN_XMMSCLIENT_H__\n#define \
__GEN_XMMSCLIENT_H__\n\n")
    xmmsclientfile.write("#include \"xmmsclient_conn.h\"\n\n")

    nodes = doc.getElementsByTagName("ipc")
    if nodes[0].nodeName == "ipc":
        print "Parsing XMMS2 IPC XML Description file version %s..." % \
        nodes[0].getAttribute("version")
    else:
        print "Error, XML files not IPC Description file."

    #parse all the enumerations
    print "Parsing enumerations"
    enum_list = nodes[0].getElementsByTagName("enum")
    do_enums(enum_list)

    #parse all objects
    print "Parsing objects"
    cfile.write("#include <stdlib.h>\n")
    object_list = nodes[0].getElementsByTagName("object")
    do_objects(object_list)

    xmmsclientfile.write("#endif\n")
