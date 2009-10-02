import struct
 
class valuedmethod(object):
    def __init__(self, f):
        self._f = f
    def __get__(self, obj, oclass=None):
        if obj:
            return lambda *args, **kw: self._f(obj, obj.value, *args, **kw)
        else:
            return lambda x, *args, **kw: self._f(oclass, x, *args, **kw)

class XMMSType(object):
    def __init__(self, value):
        self.value = value

    @classmethod
    def get_type_by_id(self, id):
        for a in self.__subclasses__():
            if a.ID == id:
                return a

    @valuedmethod
    def pack(self, o):
        return struct.pack(self.STRUCT, o)

    @valuedmethod
    def unpack(self, ins, offset=0):
        noffset = offset+struct.calcsize(self.STRUCT)
        vstr = ins[offset:noffset]
        return (struct.unpack(self.STRUCT, vstr)[0], noffset)

class NONE(XMMSType):
    ID = 0

    @valuedmethod
    def pack(self, o):
        return

    @valuedmethod
    def unpack(self, o):
        return


class UINT32(XMMSType):
    ID = 2
    STRUCT = '>I'

class INT32(XMMSType):
    ID = 3
    STRUCT = '>i'

class FLOAT(XMMSType):
    ID = None
    STRUCT = '>f'

class CHAR(XMMSType):
    ID = None
    STRUCT = '>c'

class STRING(XMMSType):
    ID = 4
    STRUCT = '>%dc'

    @valuedmethod
    def pack(self, string):
        string = string + '\x00'
        payload = UINT32.pack(len(string))
        payload += struct.pack(self.STRUCT % len(string), *string)
        return payload

    @valuedmethod
    def unpack(self, ins, offset=0):
        length, offset = UINT32.unpack(ins, offset)
        noffset = offset+struct.calcsize(self.STRUCT  % length)
        string = ins[offset:noffset]
        
        ret = ''.join(struct.unpack(self.STRUCT % length, string)[:-1])
        return (ret, noffset)

class ERROR(STRING):
    ID = 1

class COLL(XMMSType):
    ID = 5

class BIN(STRING):
    ID = 6

class LIST(XMMSType):
    ID = 7

    @valuedmethod
    def pack(self, array):
        payload = UINT32.pack(len(array))
        for a in array:
            payload += AType(a)
        return payload

    @valuedmethod
    def unpack(self, ins, offset=0):
        length, offset = UINT32.unpack(ins, offset)
        out = []
        while length:
            v, offset = AType.unpack(ins, offset)
            out.append(v)
            length -= 1
        return (out, offset)

class DICT(XMMSType):
    ID = 8
    @valuedmethod
    def pack(self, hashmap):
        payload = UINT32.pack(len(hashmap))
        for k,v in hashmap.iteritems():
            payload += STRING.pack(k)
            payload += AType.pack(v)
        return payload

    @valuedmethod
    def unpack(self, ins, offset=0):
        length, offset = UINT32.unpack(ins, offset)
        out = {}
        while length:
            k, offset = STRING.unpack(ins, offset)
            v, offset = AType.unpack(ins, offset)
            out[k] = v
            length -= 1
        return (out, offset)

class AType(XMMSType):
    ID = None

    @valuedmethod
    def pack(self, a):
        payload = INT32.pack(a.ID)
        payload += a.pack()
        return payload

    @valuedmethod
    def unpack(self, ins, offset=0):
        vtype, offset = INT32.unpack(ins, offset)
        return XMMSType.get_type_by_id(vtype).unpack(ins, offset)

