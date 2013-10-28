#!/usr/bin/env python


def parse_log(log_filename, tab_filename):
    with open(log_filename) as f:
        doc_num = None
        rejuvenate = []
        docs = []
        num_particles = None
        for line in f:
            line = line.strip()
            if line.startswith('DOCUMENT'):
                doc_num = int(line.split()[1])
            elif line.startswith('REJUVENATE'):
                token = int(line.split()[1])
                rejuvenate.append((doc_num, token))
            elif line.startswith('TIMEPERWORD'):
                time_per_word = int(line.split()[1])
            elif line.startswith('NUMWORDS '):
                num_words = int(line.split()[1])
            elif line.startswith('Array('):
                if doc_num is not None:
                    nmi = [float(x) for x in
                        line[len('Array('):len(line)-1].split(', ')]
                    num_particles = len(nmi)
                    docs.append([doc_num, time_per_word, num_words] + nmi)
            elif line.startswith('WRITE TOPICS'):
                break

    with open(tab_filename, 'w') as f:
        f.write('doc.num\ttime.per.word\tnum.words\t' +
            '\t'.join('nmi.%d' % p for p in range(num_particles)) +
            '\n')
        for doc in docs:
            f.write('\t'.join([str(x) for x in doc]) + '\n')


if __name__ == '__main__':
    import sys
    parse_log(*sys.argv[1:])
