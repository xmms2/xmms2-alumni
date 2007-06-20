#!/usr/bin/python
import sys
import string
import xml.dom.minidom

#dictionary mapping of types in the xml to C types
c_map = {}
c_map["none"] = "void"
c_map["int"] = "gint"
c_map["string"] = "char *"
c_map["enum"] = "guint"

objects = ()

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
                xmmsclientfile.write("\tXMMSC_%s_%s,\n" % \
                                     (node.getAttribute("name").upper(),
                                     prop.getAttribute("name")))
            xmmsclientfile.write("} xmmsc_%s_t; \n\n" % \
                    node.getAttribute("name"))
            print "done"

def do_objects(objects):
    for node in objects:
        #if its a child of the main(ipc) tag
        if node.parentNode == nodes[0]:
            sys.stdout.write("%s..." % node.getAttribute("name"))

            if node.getAttribute("type") == "client":
                xmmsclientfile.write("\n/* %s object */\n" % \
                        node.getAttribute("name"))

                #enums (properties/variables)
                xmmsclientfile.write("typedef enum {\n")

                props = node.getElementsByTagName("prop")
                for prop in props:
                    type = node.getElementsByTagName("type")
#                   xmmsclientfile.write("\t%s %s;\n" % \
#                                        (c_map[type[0].childNodes[1].nodeName],
#                                        prop.getAttribute("name")))

                    xmmsclientfile.write("\tXMMSC_%s_PROPERTY_%s,\n" % \
                                         (node.getAttribute("name").upper(),
                                         prop.getAttribute("name").upper()))
                xmmsclientfile.write("} xmmsc_%s_properties_t;\n\n" % \
                        node.getAttribute("name"))

                #method declarations
                methods = node.getElementsByTagName("method")
                for method in methods:
                    retval = method.getElementsByTagName("retval")
                    xmmsclientfile.write("%s " % \
                        (c_map[retval[0].childNodes[1].childNodes[1].nodeName]))

                    #figure out how to write the arguments
                    argstring = "xmmsc_connection_t *c"

                    args = method.getElementsByTagName("arg")
                    if args.length > 0:
                        for arg in args:
                            type = arg.getElementsByTagName("type")
                            type = type[0].childNodes[1].nodeName
                            name = arg.getAttribute("name")
                            argstring = argstring + ", " + c_map[type] + " " + name

                    argstring = argstring + ", xmmsc_error_t *err"

                    #actually output the rest of the line
                    xmmsclientfile.write("xmmsc_%s_%s (%s);\n" % \
                                         (node.getAttribute("name"),method.getAttribute("name"),
                                         argstring))

            else:
                print "asdf"

            print "done"

if __name__ == "__main__":

    #load the xml file
    doc = xml.dom.minidom.parse("ipc.xml")

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
    do_enums(enum_list)

    #parse all objects
    print "Parsing objects"
    object_list = nodes[0].getElementsByTagName("object")
    do_objects(object_list)

