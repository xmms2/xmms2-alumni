#!/usr/bin/env python

import struct
import asyncore
import socket
import os

from xmmstypes import *

SOCKET = '/tmp/xmms-ipc-alex'
PROTOCOL_VERSION=12
# object, cmd, cookie, length
HEADER = '>IIII'

class XMMSClient(asyncore.dispatcher, object):
    def __init__(self, clientname):
        super(XMMSClient, self).__init__()
        self.clientname = clientname
        self.buffer = ''
        self.cookie_to_cmd = {}

    def _get_header(self, resp):
        return struct.unpack(HEADER, resp)

    def _send_message(self, cmd, cb, *args):
        msg = cmd.request(*args)
        print msg['cookie']
        self.cookie_to_cmd[msg['cookie']] = {'cmd':cmd, 'cb':cb}
        self.buffer += msg['data']

    def connect(self, sock, cb=None):
        self.create_socket(socket.AF_UNIX, socket.SOCK_STREAM)
        super(XMMSClient, self).connect(sock)
        self._send_message(XMMSObjectMain.hello, cb, PROTOCOL_VERSION, 
            self.clientname)

    def playlist_current_pos(self, playlist='_active', cb=None):
        self._send_message(XMMSObjectPlaylist.current_pos, cb, playlist)

    def playback_current_id(self, cb=None):
        self._send_message(XMMSObjectOutput.current_id, cb)

    def playback_current_playtime(self, cb=None):
        self._send_message(XMMSObjectOutput.cplaytime, cb)

    def handle_connect(self):
        pass

    def handle_close(self):
        self.close()

    def handle_read(self):
        data = self.recv(16)
        resp = self._get_header(data)
        data = self.recv(resp[3])
        if resp[1] == 2:
            cookiecb = self.cookie_to_cmd[resp[2]]
            rtype = INT32.unpack(data[:4])
            if cookiecb['cb']:
                cookiecb['cb'](cookiecb['cmd'].response(data[4:]))

    def handle_write(self):
        sent = self.send(self.buffer)
        self.buffer = self.buffer[sent:]


class msg(object):
    def __init__(self, ipc_cmd, returns, args=[]):
        self.obj = None
        self.ipc_cmd = ipc_cmd
        self.returns = returns
        self.args = args

    def __get__(self, obj, oclass=None):
        if not self.obj:
            self.obj = oclass
        return self

    def response(self, instream):
        return self.returns.unpack(instream)
        
    def request(self, *args):
        body = ''
        for packer, arg in zip(self.args, args):
            body += packer.pack(arg)
        leng = len(body)
        cookie = XMMSMessenger.COOKIE
        header = struct.pack(HEADER, self.obj.ID, self.ipc_cmd, cookie, leng)
        data = header+body
        XMMSMessenger.COOKIE += 1
        return {'data':data, 'cookie':cookie}

class XMMSMessenger(object):
    COOKIE = 0

class XMMSObjectMain(XMMSMessenger):
    ID = 0

    hello = msg(ipc_cmd=0, returns=NONE, args=[UINT32, STRING])
    quit = msg(ipc_cmd=1, returns=NONE)
    plugin_list = msg(ipc_cmd=4, returns=LIST, args=[UINT32])
    stats = msg(ipc_cmd=5, returns=DICT)

class XMMSObjectPlaylist(XMMSMessenger):
    ID = 1

    shuffle = msg(ipc_cmd=6, returns=NONE, args=[STRING])
    set_pos = msg(ipc_cmd=7, returns=UINT32, args=[UINT32])
    set_pos_rel = msg(ipc_cmd=8, returns=UINT32, args=[INT32])
    add_url = msg(ipc_cmd=9, returns=NONE, args=[STRING, STRING])
    add_id = msg(ipc_cmd=10, returns=NONE, args=[STRING, UINT32])
    add_idlist = msg(ipc_cmd=11, returns=NONE, args=[STRING, COLL])
    add_coll = msg(ipc_cmd=12, returns=NONE, args=[STRING, COLL, LIST])
    remove_entry = msg(ipc_cmd=13, returns=NONE, args=[STRING, UINT32])
    move_entry = msg(ipc_cmd=14, returns=NONE, args=[STRING, UINT32, UINT32])
    clear = msg(ipc_cmd=15, returns=NONE, args=[STRING])
    sort = msg(ipc_cmd=16, returns=NONE, args=[STRING, LIST])
    list = msg(ipc_cmd=17, returns=LIST, args=[STRING])
    current_pos = msg(ipc_cmd=18, returns=DICT, args=[STRING])
    current_active = msg(ipc_cmd=19, returns=STRING)
    insert_url = msg(ipc_cmd=20, returns=NONE, args=[STRING, UINT32, STRING])
    insert_id = msg(ipc_cmd=21, returns=NONE, args=[STRING, UINT32, UINT32])
    insert_coll = msg(ipc_cmd=22, returns=NONE, args=[STRING, UINT32, COLL, LIST])
    load = msg(ipc_cmd=23, returns=NONE, args=[STRING])
    radd = msg(ipc_cmd=24, returns=NONE, args=[STRING, STRING])
    rinsert = msg(ipc_cmd=25, returns=NONE, args=[STRING, UINT32, STRING])
    
class XMMSObjectConfig(XMMSMessenger):
    ID = 2

    getvalue = msg(ipc_cmd=26, returns=STRING, args=[STRING])
    setvalue = msg(ipc_cmd=27, returns=NONE, args=[STRING, STRING])
    regvalue = msg(ipc_cmd=28, returns=STRING, args=[STRING, STRING])
    listvalues = msg(ipc_cmd=29, returns=DICT)

class XMMSObjectOutput(XMMSMessenger):
    ID = 3

    start = msg(ipc_cmd=30, returns=NONE)
    stop = msg(ipc_cmd=31, returns=NONE)
    pause = msg(ipc_cmd=32, returns=NONE)
    decoder_kill = msg(ipc_cmd=33, returns=NONE)
    cplaytime = msg(ipc_cmd=34, returns=UINT32)
    seekms = msg(ipc_cmd=35, returns=NONE, args=[UINT32])
    seekms_rel = msg(ipc_cmd=36, returns=NONE, args=[INT32])
    seeksamples = msg(ipc_cmd=37, returns=NONE, args=[UINT32])
    seek_samples_rel = msg(ipc_cmd=38, returns=NONE, args=[INT32])
    output_status = msg(ipc_cmd=39, returns=UINT32)
    current_id = msg(ipc_cmd=40, returns=UINT32)
    volume_set = msg(ipc_cmd=41, returns=NONE, args=[STRING, UINT32])
    volume_get = msg(ipc_cmd=42, returns=DICT)
    
class XMMSObjectMedialib(XMMSMessenger):
    ID = 4
    
    info = msg(ipc_cmd=43, returns=DICT, args=[UINT32])
    path_import = msg(ipc_cmd=44, returns=NONE, args=[STRING])
    rehash = msg(ipc_cmd=45, returns=NONE, args=[UINT32])
    get_id = msg(ipc_cmd=46, returns=UINT32, args=[STRING])
    remove_id = msg(ipc_cmd=47, returns=NONE, args=[UINT32])
    property_set_str = msg(ipc_cmd=48, returns=NONE, args=[UINT32, STRING, STRING, STRING])
    property_set_int = msg(ipc_cmd=49, returns=NONE, args=[UINT32, STRING, STRING, INT32])
    property_remove = msg(ipc_cmd=50, returns=NONE, args=[UINT32, STRING, STRING])
    move_url = msg(ipc_cmd=51, returns=NONE, args=[UINT32, STRING])

class XMMSObjectCollection(XMMSMessenger):
    ID = 5

    collection_get = msg(ipc_cmd=52, returns=COLL, args=[STRING, STRING])
    collection_list = msg(ipc_cmd=53, returns=LIST, args=[STRING])
    collection_save = msg(ipc_cmd=54, returns=NONE, args=[STRING, STRING, COLL])
    collection_remove = msg(ipc_cmd=55, returns=NONE, args=[STRING, STRING])
    collection_find = msg(ipc_cmd=56, returns=LIST, args=[UINT32, STRING])
    collection_rename = msg(ipc_cmd=57, returns=NONE, args=[STRING, STRING, STRING])
    query_ids = msg(ipc_cmd=58, returns=LIST, args=[COLL, UINT32, UINT32, LIST])
    query_infos = msg(ipc_cmd=59, returns=LIST, args=[COLL, UINT32, UINT32, LIST, LIST, LIST])
    idlist_from_pls = msg(ipc_cmd=60, returns=COLL, args=[STRING])
    collection_sync = msg(ipc_cmd=61, returns=NONE)

class XMMSObjectSignal(XMMSMessenger):
    ID = 6

class XMMSObjectService(XMMSMessenger):
    ID = 7
    
    MSGS = {'service_register':62, 'service_unregister':63, 'service_list':64,
        'service_describe':65, 'service_query':66, 'service_return':67,
        'service_shutdown':68}

class XMMSObjectVisualization(XMMSMessenger):
    ID = 8

class XMMSObjectMediainfoReader(XMMSMessenger):
    ID = 9

class XMMSObjectXForm(XMMSMessenger):
    ID = 10

    browse = msg(ipc_cmd=70, returns=LIST, args=[STRING])

class XMMSObjectBindata(XMMSMessenger):
    ID = 11

    get_data = msg(ipc_cmd=71, returns=BIN, args=[STRING])
    add_data = msg(ipc_cmd=72, returns=STRING, args=[BIN])
    remove_data = msg(ipc_cmd=73, returns=NONE, args=[STRING])
    list_data = msg(ipc_cmd=74, returns=LIST)
