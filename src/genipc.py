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

#set of strings, not the node objects
objectlist = set()

#map of node objects corresponding to the root node of methods of objects
methodmap = {}

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

#write header of the ipc_msg_deserialize.c file
def start_deser():
    ipcmsgdeser.write("/* generated by genipc.py */\n\n")
    #include files go here

def output_deserialize_server(obj,method):
    ipcmsgdeser.write("void\ndeserialize_call_%s_cmd_%s (xmms_ipc_t *ipc, \
xmms_ipc_msg_t *msg)\n{\n\n" % (obj.getAttribute("name"),
			    method.getAttribute("name")))

    #for each argument to the method, get the type of the argument out of the
    #msg and put in a variable, then call the method

    ipcmsgdeser.write("}\n\n")

#write the header of the ipc_msg_gen.c file
def start_processmsg():
    ipcmsggen.write("/* generated by genipc.py */\n\n")
    #include files go here

    ipcmsggen.write("void\nprocess_msg (xmms_ipc_client_t *client, xmms_ipc_t\
*ipc, xmms_ipc_msg_t *msg)\n{\n\n")
    #body of func

    ipcmsggen.write("\tint obj, cmd;\n")
    ipcmsggen.write("\n\tobj = xmms_ipc_msg_get_object (msg);\n")
    ipcmsggen.write("\tcmd = xmms_ipc_msg_get_cmd (msg);\n")

    #case for each of the objects
    ipcmsggen.write("\tswitch (obj) {\n")

#output the processmsg code for an object
def write_processmsg(node):
    ipcmsggen.write("\t\tcase XMMSC_IPC_OBJECT_%s:\n" % \
	    node.getAttribute("name").upper())
    #output code that deserializes the arguments to the command, finds the
    #hooks in ipc->cmds, and calls it correctly
    for method in methodmap[node.getAttribute("name")]:
        ipcmsggen.write("\t\t\tif (cmd == XMMS_%s_CMD_%s) {\n" % \
		(node.getAttribute("name").upper(),method.getAttribute("name").upper()))
	ipcmsggen.write("\t\t\t\tdeserialize_call_%s_cmd_%s (ipc, msg)\n" %
		(node.getAttribute("name"),method.getAttribute("name")))
	ipcmsggen.write("\t\t\t}\n")

	#output the deserialize function
	output_deserialize_server(node,method)

    ipcmsggen.write("\t\t\tbreak;\n")

#output code that takes care of xmms_cmd structs
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

    #add an entry in GPtrArray for the cmds(indexed by XMMSC_IPC_OBJECT_XXX)
    cfile.write("\tipc->cmds = g_list_insert (ipc->cmds, (gpointer) cmds, XMMSC_IPC_OBJECT_%s);\n" % node.getAttribute("name"))

    cfile.write("}\n\n")

#deal with enums
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

#deal with objects
#TODO this should probably be split up, its quite long
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
		methodmap[node.getAttribute("name")] = set()

		for method in methods:
		    #add to the methodmap
		    methodmap[node.getAttribute("name")].add(method)
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
		methodmap[node.getAttribute("name")] = set()

		for method in methods:
		    #add to the methodmap
		    methodmap[node.getAttribute("name")].add(method)
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

		#now that we have the methods we can write the processmsg code
		write_processmsg(node)

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

#start here
if __name__ == "__main__":

    #load the xml file
    doc = xml.dom.minidom.parse("ipc.xml")

    #open main output files
    xmmsclientfile = open("genipc_out/xmmsclient.h","w+")
    ipcmsggen = open("genipc_out/ipc_msg_gen.c","w+")
    ipcmsgdeser = open("genipc_out/ipc_msg_deserialize.c","w+")

    start_processmsg()
    start_deser()

    #header guard and include file for xmmsclient.h
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

    #close the header guard on xmmsclient.h
    xmmsclientfile.write("#endif\n")
