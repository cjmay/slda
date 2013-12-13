#!/usr/bin/env python


import os.path
import sys
import re


LOG_STEM = 'slda.o'

PER_DOC_STAT_NAMES = (
    'IN-SAMPLE NMI',
    'IN-SAMPLE-NON-INIT NMI',
    'OUT-OF-SAMPLE NMI',
    'OUT-OF-SAMPLE PERPLEXITY',
    'OUT-OF-SAMPLE LOG-LIKELIHOOD',
    'TIMEPERWORD',
)

BADNESS_RE = re.compile(r'\W')


class LogData(object):
    def __init__(self, num_words, per_doc_stats):
        self.num_words = num_words
        self.per_doc_stats = per_doc_stats


class AggLogData(object):
    def __init__(self):
        self.num_words = []
        self.per_doc_stats = dict((normalize_stat_name(name), []) for name in PER_DOC_STAT_NAMES + ('num_rejuvenations',))

    def num_runs(self, stat_name):
        stats = self.per_doc_stats[stat_name]
        if stats:
            return len(stats[0])
        else:
            return 0

    def make_na(self, n):
        return ['NA' for i in range(n)]

    def add(self, log_data):
        self.num_words += log_data.num_words[len(self.num_words):]

        for stat_name in self.per_doc_stats:
            run_stats = log_data.per_doc_stats[stat_name]
            stats = self.per_doc_stats[stat_name]

            # Adjust for any differences in lengths
            if len(stats) == 0:
                # initialize to empty arrays
                stats += [[] for i in range(len(run_stats))]
            elif len(run_stats) > len(stats):
                # pad stats with arrays of NA records
                pad_len = len(run_stats) - len(stats)
                stats += [self.make_na(self.num_runs(stat_name)) for j in range(pad_len)]
            elif len(run_stats) < len(stats):
                # pad run_stats with NA records
                pad_len = len(stats) - len(run_stats)
                run_stats += self.make_na(pad_len)

            # Add (adjusted) run stats to (adjusted) aggregate stats
            for i in range(len(run_stats)):
                stats[i].append(run_stats[i])

    def write(self, filename_template):
        filename = filename_template % normalize_stat_name('num_words')
        with open(filename, 'w') as f:
            for n in self.num_words:
                f.write(str(n) + '\n')

        for stat_name in self.per_doc_stats:
            stats = self.per_doc_stats[stat_name]
            filename = filename_template % normalize_stat_name(stat_name)
            with open(filename, 'w') as f:
                f.write('\t'.join(('run.%d' % i) for i in range(self.num_runs(stat_name))) + '\n')
                for iter_stats in stats:
                    f.write('\t'.join(str(x) for x in iter_stats) + '\n')
            


def normalize_stat_name(name):
    return BADNESS_RE.subn('_', name.lower())[0]


def process_logs(experiment_path):
    print(experiment_path)
    if not os.path.isdir(experiment_path):
        raise Exception(experiment_path + ' is not a directory')
    for dataset_entry in os.listdir(experiment_path):
        agg_log_data = AggLogData()
        dataset_path = os.path.join(experiment_path, dataset_entry)
        if os.path.isdir(dataset_path):
            for entry in os.listdir(dataset_path):
                path = os.path.join(dataset_path, entry)
                if os.path.isfile(path) and entry.startswith(LOG_STEM):
                    agg_log_data.add(parse_log(path))
            agg_log_data.write(dataset_path + '_%s.tab')


def parse_log(log_filename):
    with open(log_filename) as f:
        doc_rejuvenations = None
        num_words = []
        per_doc_stats = dict((normalize_stat_name(name), []) for name in PER_DOC_STAT_NAMES + ('num_rejuvenations',))
        for line in f:
            line = line.strip()

            split_idx = line.rfind(' ')
            if split_idx >= 0:
                key = line[:split_idx]
                val = line[split_idx+1:].strip()
            else:
                key = None
                val = None

            if line.startswith('DOCUMENT '):
                if doc_rejuvenations is not None:
                    per_doc_stats[normalize_stat_name('num_rejuvenations')].append(len(doc_rejuvenations))
                doc_rejuvenations = []
            elif key == 'REJUVENATE':
                doc_rejuvenations.append(int(val))
            elif key == 'NUMWORDS':
                num_words.append(int(val))
            elif key in PER_DOC_STAT_NAMES:
                per_doc_stats[normalize_stat_name(key)].append(val)
            elif line == 'PRINT TOPICS':
                break

    return LogData(num_words, per_doc_stats)


if __name__ == '__main__':
    process_logs(*sys.argv[1:])
