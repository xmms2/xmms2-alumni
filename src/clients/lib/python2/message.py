import struct

PROTOCOL_VERSION=16
# object, cmd, cookie, length
HEADER = '>IIII'

class msg(object):
    def __init__(self, ipc_cmd, returns, args=[]):
        self.obj = None
        self.ipc_cmd = ipc_cmd
        self.returns = returns
        self.args = args

    def __get__(self, obj, oclass=None):
        if not self.obj:
            self.obj = obj
        return self

    def __call__(self, *args, **kwargs):
        cb = kwargs.get('cb', None)
        if not cb and callable(args[-1]):
            cb = args[-1]
            args = args[:-1]
        self.obj.client._send_message(self, cb, *args)

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

    def __init__(self, client):
        self.client = client
