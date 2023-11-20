
import logging
from datetime import datetime
from os import path

from afew.filters.BaseFilter import Filter
from afew.FilterRegistry import register_filter


@register_filter
class LogFilter(Filter):
    message = 'Log received mail'
    tags = ['-new;-to_log']
    query = 'tag:to_log'

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        log_file = path.join(self.database.db_path, 'afew.log')
        self.logfile = logging.getLogger('{}.{}.file'.format(
            self.__module__, self.__class__.__name__))
        self.logfile.addHandler(logging.FileHandler(log_file))
        self.logfile.setLevel('INFO')
        self.logfile.propagate = False

    def run(self, query):
        self.log.info(self.message)

        # Don't append 'AND tag:new' to self.query
        assert getattr(self, 'query', None) is not None, "Query is None"
        query = "({})".format(self.query)

        for message in self.database.get_messages(query):
            self.handle_message(message)

    def handle_message(self, message):
        self.remove_tags(message, *self._tags_to_remove)

        try:
            date = datetime.fromtimestamp(message.get_date())
            s = (
                "id:" + message.get_message_id(),
                "from:" + message.get_header("From"),
                message.get_header("Subject"),
                date.strftime('%Y/%m/%d %H:%M'),
                str(message.get_tags())
            )
            log = "\n\t".join(s)
        except Exception:
            log = "Error trying to log message id{}".format(message.get_message_id())

        self.logfile.info(log)
