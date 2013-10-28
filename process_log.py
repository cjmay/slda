#!/usr/bin/env python


import os.path
import sys


def mean(x):
    return sum(x) / len(x)


class LogData(object):
    def __init__(self, doc_nums, time_per_word, num_words, nmi, num_particles,
            rejuvenate):
        self.doc_nums = doc_nums
        self.time_per_word = time_per_word
        self.num_words = num_words
        self.nmi = nmi
        self.num_particles = num_particles
        self.rejuvenate = rejuvenate


class AggLogData(object):
    def __init__(self):
        self.mean_nmi = []

    def add(self, log_data):
        if self.mean_nmi:
            if len(log_data.nmi) == len(self.mean_nmi):
                for i in range(len(log_data.nmi)):
                    self.mean_nmi[i].append(mean(log_data.nmi[i]))
            else:
                sys.stderr.write('Wrong number of documents: %d != %d\n' % (len(log_data.nmi), len(self.mean_nmi)))
        else:
            self.mean_nmi = [[mean(p)] for p in log_data.nmi]

    def write(self, filename):
        with open(filename, 'w') as f:
            f.write('\t'.join('run.%d' % i for i in range(len(self.mean_nmi[0]))) + '\n')
            for nmi in self.mean_nmi:
                f.write('\t'.join(str(x) for x in nmi) + '\n')
            


def process_logs(log_location, out_filename):
    agg_log_data = AggLogData()

    if os.path.isdir(log_location):
        for entry in os.listdir(log_location):
            path = os.path.join(log_location, entry)
            if os.path.isfile(path):
                d = parse_log(path)
                agg_log_data.add(d)
    else:
        path = log_location
        d = parse_log(path)
        agg_log_data.add(d)

    agg_log_data.write(out_filename)


def parse_log(log_filename):
    with open(log_filename) as f:
        doc_num = None
        rejuvenate = []
        time_per_word = []
        num_words = []
        nmi = []
        doc_nums = []
        num_particles = None
        for line in f:
            line = line.strip()
            if line.startswith('DOCUMENT'):
                doc_num = int(line.split()[1])
                doc_nums.append(doc_num)
            elif line.startswith('REJUVENATE'):
                token = int(line.split()[1])
                rejuvenate.append((doc_num, token))
            elif line.startswith('TIMEPERWORD'):
                ms = int(line.split()[1])
                time_per_word.append(ms)
            elif line.startswith('NUMWORDS '):
                num = int(line.split()[1])
                num_words.append(num)
            elif line.startswith('Array('):
                if doc_num is not None:
                    nmi_by_particle = [float(x) for x in
                        line[len('Array('):len(line)-1].split(', ')]
                    nmi.append(nmi_by_particle)
                    num_particles = len(nmi)
            elif line.startswith('WRITE TOPICS'):
                break

    return LogData(doc_nums, time_per_word, num_words, nmi, num_particles,
        rejuvenate)


if __name__ == '__main__':
    process_logs(*sys.argv[1:])
