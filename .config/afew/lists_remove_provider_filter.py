import re

from afew.filters.BaseFilter import Filter
from afew.FilterRegistry import register_filter

@register_filter
class ListsRemoveProviderFilter(Filter):
    message = 'List without provider'
    list_providers = "none;"
    query = 'tag:locean'

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        regex = r"^<(.*)\.({})>$".format('|'.join(
            self.list_providers.split(';')))
        self.regex = re.compile(regex)

    def handle_message(self, message):
        listid = message.get_header('List-Id')
        m = self.regex.match(listid)
        if m is not None:
            self.add_tags(message, 'list/{}'.format(m.group(1)))
            self.add_tags(message, *self._tags_to_add)
            self.remove_tags(message, *self._tags_to_remove)
