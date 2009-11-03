"""

 An asyncore connector -
	to use with your cool asyncore xmms2 client

 Just create the AsyncCoreConnector() class with a xmmsclient as argument

"""

import asynccore

class AyncCoreConnector(asyncore.file_dispatcher):
    def __init__(self, xc):
        self.xc = xc
        fd = os.fdopen(self.xc.get_fd())
        asyncore.file_dispatcher.__init__(self, fd)

    def __getattr__(self, attr):
        if hasattr(self.xc, attr):
            return getattr(self.xc, attr)
        raise AttributeError("'%s' objct has no attribute '%s'", % (self.__name__, attr))

    def writable(self):
        return self.xc.want_ioout()

    def handle_read(self):
        self.xc.ioin()

    def handle_write(self):
        self.xc.ioout()
