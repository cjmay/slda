#!/usr/bin/env python


import os.path
import sys


LOG_STEM = 'slda.o'


class LogData(object):
    def __init__(self, doc_nums, time_per_word, num_words, in_sample_nmi,
            in_sample_non_init_nmi, out_of_sample_nmi, rejuvenations):
        self.doc_nums = doc_nums
        self.time_per_word = time_per_word
        self.num_words = num_words
        self.in_sample_nmi = in_sample_nmi
        self.in_sample_non_init_nmi = in_sample_non_init_nmi
        self.out_of_sample_nmi = out_of_sample_nmi
        self.rejuvenations = rejuvenations


class AggLogData(object):
    def __init__(self):
        self.num_runs = 0
        self.in_sample_nmi = []

    def add(self, log_data):
        if self.in_sample_nmi:
            if len(log_data.in_sample_nmi) == len(self.in_sample_nmi):
                self.num_runs += 1
                for i in range(len(log_data.in_sample_nmi)):
                    self.in_sample_nmi[i].append(log_data.in_sample_nmi[i])
            else:
                sys.stderr.write('Wrong number of documents: %d != %d\n'
                    % (len(log_data.in_sample_nmi), len(self.in_sample_nmi)))
        else:
            self.num_runs += 1
            self.in_sample_nmi = [[p] for p in log_data.in_sample_nmi]

    def write(self, filename):
        with open(filename, 'w') as f:
            f.write('\t'.join('run.%d' % i for i in range(self.num_runs))
                + '\n')
            for nmi in self.in_sample_nmi:
                f.write('\t'.join(str(x) for x in nmi) + '\n')
            


def process_logs(logs_location):
    if not os.path.isdir(logs_location):
        raise Exception(logs_location + ' is not a directory')
    for dataset_entry in os.listdir(logs_location):
        agg_log_data = AggLogData()
        dataset_path = os.path.join(logs_location, dataset_entry)
        out_filename = dataset_path + '.tab'
        if os.path.exists(out_filename):
            raise Exception(out_filename + ' already exists')
        if not os.path.isdir(dataset_path):
            raise Exception(dataset_path + ' is not a directory')
        for run_entry in os.listdir(dataset_path):
            run_path = os.path.join(dataset_path, run_entry)
            if not os.path.isdir(run_path):
                raise Exception(run_path + ' is not a directory')
            for entry in os.listdir(run_path):
                path = os.path.join(run_path, entry)
                if os.path.isfile(path) and entry.startswith(LOG_STEM):
                    d = parse_log(path)
                    agg_log_data.add(d)
        agg_log_data.write(out_filename)


def parse_log(log_filename):
    with open(log_filename) as f:
        doc_num = None
        rejuvenations = []
        time_per_word = []
        num_words = []
        in_sample_nmi = []
        in_sample_non_init_nmi = []
        out_of_sample_nmi = []
        doc_nums = []
        for line in f:
            line = line.strip()
            if line.startswith('DOCUMENT'):
                doc_num = int(line.split()[1])
                doc_nums.append(doc_num)
            elif line.startswith('REJUVENATE'):
                token = int(line.split()[1])
                rejuvenations.append((doc_num, token))
            elif line.startswith('TIMEPERWORD'):
                ms = int(line.split()[1])
                time_per_word.append(ms)
            elif line.startswith('NUMWORDS '):
                num = int(line.split()[1])
                num_words.append(num)
            elif line.startswith('IN-SAMPLE NMI '):
                nmi = float(line.split()[2])
                in_sample_nmi.append(nmi)
            elif line.startswith('IN-SAMPLE-NON-INIT NMI '):
                nmi = float(line.split()[2])
                in_sample_non_init_nmi.append(nmi)
            elif line.startswith('OUT-OF-SAMPLE NMI '):
                nmi = float(line.split()[2])
                out_of_sample_nmi.append(nmi)
            elif line.startswith('WRITE TOPICS'):
                break

    return LogData(doc_nums, time_per_word, num_words, in_sample_nmi,
        in_sample_non_init_nmi, out_of_sample_nmi, rejuvenations)


if __name__ == '__main__':
    process_logs(*sys.argv[1:])
