import sys
from IPython.core import release

c.InteractiveShellApp.exec_lines = [
    # '%load_ext ipython_autoimport',
    '%load_ext autoreload',
    '%aimport -sys, -builtins, -types, -numpy, -scipy, -matplotlib, -xarray, -pandas, -dask',
    '%autoreload 2'
]

banner = 'Python {}.{}.{} | IPython {}\n'.format(
    *sys.version_info[:3], release.version
)

c.TerminalIPythonApp.display_banner = True
c.InteractiveShell.banner1 = banner

c.InteractiveShell.colors = 'LightBG'

c.TerminalInteractiveShell.confirm_exit = False
c.TerminalInteractiveShell.prompts_class = 'IPython.terminal.prompts.ClassicPrompts'
