#!/usr/bin/env python

import struct
import asyncore
import socket
import os

from xmmstypes import *
from message import PROTOCOL_VERSION, HEADER
import generated

SOCKET = '/tmp/xmms-ipc-alex'

class XMMSClient(asyncore.dispatcher, object):
    def __init__(self, clientname):
        super(XMMSClient, self).__init__()
        self.clientname = clientname
        self.buffer = ''
        self.cookie_to_cmd = {}

    def _get_header(self, resp):
        return struct.unpack(HEADER, resp)

    def __getattr__(self, attr):
        if attr in self.__dict__:
            return self.__dict__[attr]
        elif 'XMMSObject'+attr in dir(generated):
            return getattr(generated, 'XMMSObject'+attr)(self)
        else:
            return None

    def _send_message(self, cmd, cb, *args):
        msg = cmd.request(*args)
        print msg['cookie']
        self.cookie_to_cmd[msg['cookie']] = {'cmd':cmd, 'cb':cb}
        self.buffer += msg['data']

    def connect(self, sock, cb=None):
        self.create_socket(socket.AF_UNIX, socket.SOCK_STREAM)
        super(XMMSClient, self).connect(sock)
        self.Main.hello(PROTOCOL_VERSION, self.clientname, cb=cb)

    def handle_connect(self):
        pass

    def handle_close(self):
        self.close()

    def handle_read(self):
        data = self.recv(16)
        resp = self._get_header(data)
        print resp
        data = self.recv(resp[3])
        if resp[1] == 0:
            cookiecb = self.cookie_to_cmd[resp[2]]
            rtype = INT32.unpack(data[:4])
            if cookiecb['cb']:
                cookiecb['cb'](cookiecb['cmd'].response(data[4:]))

    def handle_write(self):
        sent = self.send(self.buffer)
        self.buffer = self.buffer[sent:]
