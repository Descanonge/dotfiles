
import time
import subprocess

from tkinter import Tk, BooleanVar
from tkinter import ttk


def get_callback(button):
    def callback(*args):
        button.invoke()
    return callback


def launch(*args):
    root.quit()

    for v, a in zip(variables, apps):
        a.active = v.get()
        if a.active:
            a.launch()

    # time.sleep(5)
    # for a in apps:
    #     if a.active:
    #         a.move_desktop()


class App:
    def __init__(self, name, key, active, cmd, desktop):
        self.name = name
        self.key = key.lower()
        self.active = active
        self.desktop = int(desktop)

        if not isinstance(cmd, list):
            cmd = [cmd]
        self.cmd = cmd

    def launch(self):
        subprocess.Popen(self.cmd)

    def move_desktop(self):
        subprocess.run(['wmctrl', '-r', self.name, '-t', str(self.desktop-1)])


firefox_urls = [
    'jzargo.local/welcome',
    'www.esa.int/Applications/Observing_the_Earth',
    'music.youtube.com',
    'web.whatsapp.com',
    'www.facebook.com/messages',
]

apps = [
    App('Emacs', 'e', True, 'emacs', 3),
    App('Firefox', 'f', True, ['firefox'] + firefox_urls, 2),
    App('Discord', 'd', True, 'discord', 9),
    App('Zotero', 'z', False, ['gtk-launch', 'zotero'], 8)
]

root = Tk()
root.title = "Launcher"

mainframe = ttk.Frame(root)

variables = [BooleanVar(value=a.active) for a in apps]
buttons = [ttk.Checkbutton(mainframe, text=a.name, variable=v)
           for a, v in zip(apps, variables)]

ok_btn = ttk.Button(mainframe, text='Ok', command=launch)

mainframe.grid(column=0, row=0)
for i, b in enumerate(buttons + [ok_btn]):
    b.grid(column=0, row=i)
ok_btn.grid(column=0, row=len(apps)+1)

for a, b in zip(apps, buttons):
    root.bind('<Key-{}>'.format(a.key), get_callback(b))
root.bind('<Key-Return>', launch)

s = ttk.Style()
s.theme_use('clam')
root.mainloop()
