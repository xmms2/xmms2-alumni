#!/usr/bin/python
import sys

sys.path.append('../waftools')
from genipc_server import build

build('config', 'xmms_config_t *')
