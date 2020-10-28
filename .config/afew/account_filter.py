from afew.filters.BaseFilter import Filter
from afew.FilterRegistry import register_filter

@register_filter
class AccountFilter(Filter):
    message = 'Tag email accounts'
    account = 'default'
    query = ''

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.query = ('(path:"{0}/**" OR path:"archives/{0}/**")'
                      'AND NOT tag:{0}').format(self.account)

    def handle_message(self, message):
        self.add_tags(message, self.account)
        self.add_tags(message, *self._tags_to_add)
        self.remove_tags(message, *self._tags_to_remove)
