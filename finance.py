#!/usr/bin/env python

from collections import defaultdict
from csv import reader
from sys import argv, stdin

def process(data):
    cat_to_criteria = {
        'john': ('withdrwl 736 ne mlk', 'music', 'cup & bar',),
        'caroll': ('nords', 'ross', 'macy', 'tj', 'ulta', 'sally',
                   'goodwill', 'marshal', 'lampson & valley', 'juice beauty'),
        'amazon': ('amazon',),
        'food': ('wholefds', 'albert', 'gril', 'krugers', 'ying',
                 'volcano', 'green zebra', 'noodle house', 'pearson',
                 'blind onion', 'chuck', 'safeway', 'sheridan', 'harlow',
                 'red robin', 'mother', 'starbuck', 'peruvian roti',
                 'del taco',  'baja fish', 'food in bloom'),
        'trimet': ('trimet', 'streetcar', ),
        'car': ('shell', 'oil can henry', 'arco', ),
        'ent' : ('living room theater',),
        'income': ('action', ),
        'ignore': ('powel', 'timber',
                   'coca cola', 'shilo', 'lees wok', 'walgreen',
                   'symphony', 'online banking',
                   'cleaner', 'godaddy', 'tax refund', 'h&r block', 'tax bd',
                   'tax ref', 'spectacle', 'newport rest', 'jetblue',
                    'ace', 'aat lloyd',
        ),
        'bills': ('at&t', 'pacific power',
                  'fibersphere',
        ),
        'check': ('check ',),
        'health': ('united pacific', 'kwan-yin', 'way medical'),
    }
    cats = defaultdict(list)
    for date, desc, amt, bal in data:
        key = 'no cat'
        for cat, criteria in cat_to_criteria.items():
            for c in criteria:
                if c in desc.lower():
                    key = cat
        if float(amt) > 100000: continue
        cats[key].append((date, amt, desc))
    return cats

def report(info):
    for x in sorted(
            [(k, len(v), sum(int(float(x[1])) for x in v))
             for k, v in info.items()],
            key=lambda x: x[2]):
        print(x)
# for trans in info['no cat']: print(trans[2])
with open(argv[1]) if len(argv) > 1 else stdin as f:
    report(process(reader(f)))
