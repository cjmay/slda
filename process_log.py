#!/usr/bin/env python


import os.path
import sys


LOG_STEM = 'slda.o'

PER_DOC_STAT_NAMES = (
    'IN-SAMPLE NMI',
    'IN-SAMPLE-NON-INIT NMI',
    'OUT-OF-SAMPLE NMI',
    'OUT-OF-SAMPLE PERPLEXITY',
    'OUT-OF-SAMPLE LOG-LIKELIHOOD',
    'TIMEPERWORD',
)


class LogData(object):
    def __init__(self, doc_nums, rejuvenations, num_words, per_doc_stats):
        self.doc_nums = doc_nums
        self.rejuvenations = rejuvenations
        self.num_words = num_words
        self.per_doc_stats = per_doc_stats


class AggLogData(object):
    BADNESS_RE = re.compile(r'\W')

    def __init__(self):
        self.per_doc_stats = dict((stat_name, []) for stat_name in PER_DOC_STAT_NAMES)

    def normalize_stat_name(self, name):
        return BADNESS_RE.subn('_', name.lower())

    def num_runs(self):
        if stats:
            return len(stats[0])
        else:
            return 0

    def make_na(self, n):
        return ['NA' for i in range(n)]

    def add(self, log_data):
        for stat_name in per_doc_stats:
            run_stats = log_data.per_doc_stats[stat_name]
            stats = self.per_doc_stats[stat_name]

            # Adjust for any differences in lengths
            if len(stats) == 0:
                # initialize to empty arrays
                stats += [[] for i in range(len(run_stats)]
            elif len(run_stats) > len(stats):
                # pad stats with arrays of NA records
                pad_len = len(run_stats) - len(stats)
                stats += [self.make_na(self.num_runs()) for j in range(pad_len)]
            elif len(run_stats) < len(stats):
                # pad run_stats with NA records
                pad_len = len(stats) - len(run_stats)
                run_stats += self.make_na(pad_len)

            # Add (adjusted) run stats to (adjusted) aggregate stats
            for i in range(len(run_stats)):
                stats[i].append(run_stats[i])

    def write(self, filename_template):
        for stat_name in self.per_doc_stats:
            stats = self.per_doc_stats[stat_name]
            filename = filename_template % self.normalize_stat_name(stat_name)
            with open(filename, 'w') as f:
                f.write('\t'.join('run.%d' % i for i in range(self.num_runs())) + '\n')
                for iter_stats in stats:
                    f.write('\t'.join(str(x) for x in iter_stats) + '\n')
            


def process_logs(experiment_path):
    print(experiment_path)
    if not os.path.isdir(experiment_path):
        raise Exception(experiment_path + ' is not a directory')
    for dataset_entry in os.listdir(experiment_path):
        agg_log_data = AggLogData()
        dataset_path = os.path.join(experiment_path, dataset_entry)
        if not os.path.isdir(dataset_path):
            raise Exception(dataset_path + ' is not a directory')
        for entry in os.listdir(dataset_path):
            path = os.path.join(dataset_path, entry)
            if os.path.isfile(path) and entry.startswith(LOG_STEM):
                agg_log_data.add(parse_log(path))
        agg_log_data.write(dataset_path + '_%s.tab')


def parse_log(log_filename):
    with open(log_filename) as f:
        doc_num = None
        doc_nums = []
        rejuvenations = []
        num_words = []
        per_doc_stats = dict((stat_name, []) for stat_name in PER_DOC_STAT_NAMES)
        for line in f:
            line = line.strip()

            split_idx = line.rfind(' ')
            if split_idx >= 0:
                key = line[:split_idx]
                val = line[split_idx+1:].strip()
            else:
                key = None
                val = None

            if key == 'DOCUMENT':
                doc_num = int(val)
                doc_nums.append(doc_num)
            elif key == 'REJUVENATE':
                rejuvenations.append((doc_num, int(val)))
            elif key == 'NUMWORDS':
                num_words.append(int(val))
            elif key in PER_DOC_STAT_NAMES:
                per_doc_stats[key].append(float(val))
            elif line == 'WRITE TOPICS':
                break

    return LogData(doc_nums, rejuvenations, num_words, **per_doc_stats)


if __name__ == '__main__':
    process_logs(*sys.argv[1:])
