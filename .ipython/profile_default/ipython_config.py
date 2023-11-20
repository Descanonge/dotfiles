import sys
from IPython.core import release

c.InteractiveShellApp.exec_lines = [
    '%matplotlib qt5',
    '%load_ext autoreload',
    # '%load_ext ipython_autoimport',
    '%aimport -sys, -builtins, -types, -numpy, -scipy, -matplotlib, -xarray, -pandas, -dask',
    '%autoreload -p 2',
]

banner = 'Python {}.{}.{} | IPython {}\n'.format(
    *sys.version_info[:3], release.version
)

c.TerminalIPythonApp.display_banner = True
c.InteractiveShell.banner1 = banner

c.InteractiveShell.colors = 'LightBG'

c.TerminalInteractiveShell.confirm_exit = False
c.TerminalInteractiveShell.prompts_class = 'IPython.terminal.prompts.ClassicPrompts'
