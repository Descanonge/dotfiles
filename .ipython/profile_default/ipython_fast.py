
c.InteractiveShellApp.exec_lines = [
    '%load_ext ipython_autoimport',
    '%load_ext autoreload',
    '%aimport -sys, builtins, types, numpy, scipy, matplotlib',
    '%autoreload 2'
]
