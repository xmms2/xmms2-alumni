# Stock plugin configuration and build methods. These factor out the
# common tasks carried out by plugins in order to configure and build
# themselves.

import Options

def plugin(name, source=None, configure=False, build=False,
           build_replace=False, libs=[],
           tool='cc', broken=False, output_prio=None,
           static=False):
    def stock_configure(conf):
        if Options.options.static_plugins:
            static=True
        
        if broken:
            conf.check_message_custom('%s plugin' % name, '',
                                      'disabled (broken)')
            return
        if configure and not configure(conf):
            return
        conf.env.append_value('XMMS_PLUGINS_ENABLED', name)
        if static:
            conf.env.append_value('XMMS_PLUGINS_STATIC', name)
            conf.env["CXXDEFINES_plugin" + name] = ["XMMS_PLUGIN_DESC_SYMBOL_NAME=XMMS_PLUGIN_DESC_" + name]
            conf.env["CCDEFINES_plugin" + name] = ["XMMS_PLUGIN_DESC_SYMBOL_NAME=XMMS_PLUGIN_DESC_" + name]
        else:
            conf.env["CCDEFINES_plugin" + name] = ["XMMS_PLUGIN_DESC_SYMBOL-name=XMMS_PLUGIN_DESC"]
            conf.env["CXXDEFINES_plugin" + name] = ["XMMS_PLUGIN_DESC_SYMBOL-name=XMMS_PLUGIN_DESC"]
            
        if output_prio:
            conf.env.append_value('XMMS_OUTPUT_PLUGINS', (output_prio, name))

    def stock_build(bld):
        if name in bld.env["XMMS_PLUGINS_STATIC"]:
            static=True
            
        if static:
            obj = bld.new_task_gen(tool, 'staticlib')
            obj.install_path = None
        else:
            obj = bld.new_task_gen(tool, 'shlib')
            obj.install_path = '${PLUGINDIR}'
            obj.mac_bundle = True
            
        obj.target = 'xmms_%s' % name
        obj.includes = '../../.. ../../include'

        if source:
            obj.source = source
        else:
            obj.source = ['%s.c' % name]
        
        obj.uselib = ['glib2', 'plugin'+name] + libs

        if bld.env['xmms_shared_library'] and not static:
            obj.uselib_local = 'xmms2core'

        if build:
            build(bld, obj)

    return stock_configure, stock_build
