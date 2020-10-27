import re

from afew.filters.BaseFilter import Filter
from afew.FilterRegistry import register_filter

@register_filter
class MatchSubjectBracketsFilter(Filter):
    message = 'Match first subject brackets'
    patterns = "none;"
    query = ''
    tags = '+<1>'

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        patterns = [p.replace('**', r'[^\]]*')
                    for p in self.patterns.split(';')]
        regex = r"\[({})\]".format('|'.join(patterns))
        self.regex = re.compile(regex)

        self._match_tags(self._tags_to_add)
        self._match_tags(self._tags_to_remove)

    def _match_tags(self, tags):
        pattern = re.compile(r"^<(\d)*>$")
        for i, tag in enumerate(tags):
            m = pattern.match(tag)
            if m is not None:
                tags[i] = int(m.group(1))

    def _change_tags(self, func, message, tags, m):
        new_tags = []
        for t in tags:
            if isinstance(t, int):
                try:
                    t = m.group(t)
                except IndexError:
                    self.log.warning("Not group {} in regex {}"
                                     .format(t, m.re.pattern))
            new_tags.append(t)
        func(message, *new_tags)

    def handle_message(self, message):
        subject = message.get_header('Subject')
        m = self.regex.search(subject)
        if m is not None:
            self._change_tags(self.add_tags, message, self._tags_to_add, m)
            self._change_tags(self.remove_tags, message, self._tags_to_remove, m)
