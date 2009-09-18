#!/usr/bin/env python

import struct
import asyncore
import socket
import os

from xmmstypes import *
from generated import *

SOCKET = '/tmp/xmms-ipc-alex'
PROTOCOL_VERSION=16
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
