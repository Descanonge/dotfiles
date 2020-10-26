
import os
import imp

from Mailnag.common.i18n import _
from Mailnag.common.plugins import PLUGIN_LIB_PATH
from Mailnag.common.subproc import start_subprocess

modname = 'userscriptplugin'
mod = imp.load_source(modname, os.path.join(PLUGIN_LIB_PATH, modname + '.py'))

Parent = getattr(mod, 'UserscriptPlugin')

class UserscriptPluginCustom(Parent):
	
	def get_manifest(self):
		return (_("User Script Custom"),
				_("Runs an user defined script on mail arrival."),
				"2.0",
				"Patrick Ulbrich <zulu99@gmx.net>")


	def _run_userscript(self, new_mails):
		config = self.get_config()
		script_file = config['script_file'].strip()
		if (len(script_file) > 0) and os.path.exists(script_file):
			# script_args = [ script_file, str(len(new_mails)) ]
			script_args = [ script_file ]
			
			for m in new_mails:
				sender_name, sender_addr = m.sender
				if len(sender_addr) == 0: sender_addr = 'UNKNOWN_SENDER'
				
				script_args.append(m.account_name.lower())
				# script_args.append(sender_addr)
				# script_args.append(m.subject)
			start_subprocess(script_args)
