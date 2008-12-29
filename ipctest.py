import xmmsclient
from pprint import pprint


## this should use Drax' nice pure python stuff instead.
xc=xmmsclient.XMMS("a")
xc.connect()

class NewStyleIPC:
    def __init__(self, xc):
        self.xc = xc

        xc.newstyle_callback_set(self.cb)

        xc.newstyle({"object": "ipc",
                     "method": "introspect",
                     "cookie": "this is from introspect"})

        self.next_cookie = 1
        xc.loop()
        self.pending_calls = {}

    def cb(self, v):
        cookie = v['cookie']

        if cookie == "this is from introspect":
            print "introspect:"
            pprint(v['retval'])
            self._update_methods(v['retval'])
            self.xc.exit_loop()
            return

        if cookie not in self.pending_calls:
            print "unknown result:"
            pprint(v)
            print
            return

    def _update_methods(self, v):
        for objname, methods in v.iteritems():
            for m in methods:
                def X(o, m):
                    # this needs to do fun stuff with args. RSN
                    def _(*x):
                        c = self.next_cookie
                        self.next_cookie += 1

                        self.xc.newstyle({"object": o,
                                          "method": m,
                                          "cookie": c})

                        # create a result object and
                        #  put it in pending calls here

                    nam = ("%s_%s" % (o, m)).encode("ascii")
                    _.func_name = nam
                    return _
                f = X(objname, m["name"])
                setattr(self, f.func_name, f)

ns = NewStyleIPC(xc)
print "dir(ns):"
pprint(dir(ns))
print

ns.output_playtime()
ns.output_start()


xc.loop()
