# Copyright 2013 - 2020 Patrick Ulbrich <zulu99@gmx.net>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA.
#

import os
import imp

from gi.repository import Notify
from Mailnag.common.i18n import _
from Mailnag.common.plugins import PLUGIN_LIB_PATH
from Mailnag.common.exceptions import InvalidOperationException

modname = 'libnotifyplugin'
mod = imp.load_source(modname, os.path.join(PLUGIN_LIB_PATH, modname + '.py'))

Parent = getattr(mod, 'LibNotifyPlugin')


class LibNotifyPluginCustom(Parent):

    def get_manifest(self):
        return (_("LibNotify Notifications Custom"),
                _("Shows a popup when new mails arrive."),
                "2.0",
                "Patrick Ulbrich <zulu99@gmx.net>")

    def _notify_summary(self, new_mails, all_mails):
        summary = ""
        body = ""
        mails = new_mails
        # mails = self._prepend_new_mails(new_mails, all_mails)

        if len(self._notifications) == 0:
            self._notifications['0'] = self._get_notification(" ", None, None)
            # empty string will emit a gtk warning

        ubound = len(mails) if len(mails) <= self._max_mails else self._max_mails

        for i in range(ubound):
            if self._is_gnome:
                body += "%s:\n<i>%s</i>\n\n" % (self._get_sender(mails[i]),
                                                mails[i].subject)
            else:
                body += "%s  -  %s\n" % (ellipsize(self._get_sender(mails[i]), 20),
                                         ellipsize(mails[i].subject, 20))

        if len(mails) > self._max_mails:
            if self._is_gnome:
                body += "<i>%s</i>" % _("(and {0} more)").format(str(len(mails) - self._max_mails))
            else:
                body += _("(and {0} more)").format(str(len(mails) - self._max_mails))

        if len(mails) > 1:  # multiple new emails
            summary = _("{0} new mails").format(str(len(mails)))
        else:
            summary = _("New mail")

        self._notifications['0'].update(summary, body, "mail-unread")
        self._notifications['0'].show()

        # Mark mails as read
        controller = self.get_mailnag_controller()

        for i in range(ubound):
            try:
                controller.mark_mail_as_read(mails[i].id)
            except InvalidOperationException:
                pass

    def _notify_single(self, mails):
        # In single notification mode new mails are
        # added to the *bottom* of the notification list.
        mails.sort(key=lambda m: m.datetime, reverse=False)

        for mail in mails:
            n = self._get_notification(self._get_sender(mail) + " (%s)" % mail.account_name, mail.subject, "mail-unread")
            notification_id = str(id(n))
            if self._is_gnome:
                n.add_action("mark-as-read", _("Mark as read"),
                             self._notification_action_handler,
                             (mail, notification_id))
            n.show()
            self._notifications[notification_id] = n

    def _notify_count(self, count):
        if len(self._notifications) == 0:
            self._notifications['0'] = self._get_notification(" ", None, None)
            # empty string will emit a gtk warning

        if count > 1:  # multiple new emails
            summary = _("{0} new mails").format(str(count))
        else:
            summary = _("New mail")

        self._notifications['0'].update(summary, None, "mail-unread")
        self._notifications['0'].show()

    def _get_notification(self, summary, body, icon):
        n = Notify.Notification.new(summary, body, icon)
        n.set_category("email")
        n.set_hint_string("desktop-entry", "mailnag")
        n.set_timeout(2500)

        # if self._is_gnome:
        #     n.add_action("default", "default", self._notification_action_handler, None)

        return n

    def _get_sender(self, mail):
        name, addr = mail.sender
        if len(name) > 0:
            return name
        return addr


def ellipsize(str, max_len):
    if max_len < 3:
        max_len = 3
    if len(str) <= max_len:
        return str
    else:
        return str[0:max_len - 3] + '...'
