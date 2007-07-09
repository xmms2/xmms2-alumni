#!/bin/sh

#source
cp genipc_out/ipc_cmds.c xmms
cp genipc_out/ipc_msg_gen.c xmms
cp genipc_out/ipc_msg_deserialize.c xmms

#headers
cp genipc_out/xmmsclient.h include/xmmsclient
cp genipc_out/*cmds.h includepriv/xmmspriv

