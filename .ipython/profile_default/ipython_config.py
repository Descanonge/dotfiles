
c.InteractiveShellApp.exec_lines = [
    '%matplotlib tk',
    '%load_ext autoreload',
    '%aimport -sys, builtins, types, numpy, scipy, matplotlib',
    '%autoreload 2'
]
c.TerminalIPythonApp.display_banner = False
c.InteractiveShell.colors = 'LightBG'
c.TerminalInteractiveShell.confirm_exit = False
c.TerminalInteractiveShell.prompts_class = 'IPython.terminal.prompts.ClassicPrompts'
