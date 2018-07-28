#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import print_function, division

import json
import numpy as np
import sys

import matplotlib.pyplot as plt

FILE = 'bench.json'

BGROUPS = [
    ('set'        , 'Data.Set Word64'),
    ('hashset'    , 'Data.HashSet Word64'),
    ('containers' , 'Data.IntSet'),
    ('ffi'        , 'C/FFI'),
    ('native-div' , 'Data.Primitive.ByteArray (with div/rem)'),
    ('native'     , 'Data.Primitive.ByteArray (with shift/and)'),
    ('ghc'        , 'Using GHC.Exts (with shift/and)'),
    ]

class Report(object):
    def __init__(self, name, analysis):
        self.name = name
        self.analysis = analysis

    def group(self):
        return self.name.split('/', 1)[0]

    def configuration(self):
        return self.name.split('/', 1)[1]

    @classmethod
    def from_json(cls, o):
        analysis = ReportAnalysis.from_json(o['reportAnalysis'])
        name = o['reportName']
        return cls(name=name,
                   analysis=analysis)

class ReportAnalysis(object):
    def __init__(self, mean=None):
        self.mean = mean

    @classmethod
    def from_json(cls, o):
        mean = AnalysisMean.from_json(o['anMean'])
        return cls(mean=mean)

class AnalysisMean(object):
    def __init__(self, value, ci_ldx, ci_udx, ci_level):
        self.value = value
        self.ci_ldx = ci_ldx
        self.ci_udx = ci_udx
        self.ci_level = ci_level

    @classmethod
    def from_json(cls, o):
        e = o['estError']
        return cls(value=o['estPoint'],
                   ci_ldx=e['confIntLDX'],
                   ci_udx=e['confIntUDX'],
                   ci_level=e['confIntCL'])


def main(to_plot='summary'):
    # Read in JSON file
    with open(FILE) as fp:
        o = json.load(fp)
    bench_list = [Report.from_json(x) for x in o[2]]

    # Group benchmarks
    bench = {}
    for b in bench_list:
        try:
            bench[b.group()].append(b)
        except KeyError:
            bench[b.group()] = [b]
    for g in bench.iterkeys():
        bench[g].sort(key=lambda b: int(b.configuration()))

    # Plot
    if to_plot == 'summary':
        plot(bench)
    elif to_plot == 'all':
        plot_naive(bench)
        plot_native_div(bench)
        plot_native_fast(bench)
        plot(bench)

    else:
        raise ValueError('unknown to_plot value: {}'.format(to_plot))

def plot(bench, bgroups=BGROUPS):
    # Extract indices
    all_configs = sorted(set(b.configuration() for bs in bench.itervalues() for b in bs))
    all_configs = dict(zip(all_configs, np.arange(len(all_configs))))

    # Make the plot
    width = 0.7/len(bench)
    offset = 0
    fig, ax = plt.subplots()
    groups = []
    for g, gn in bgroups:
        try:
            bs = bench[g]
        except KeyError:
            continue

        groups.append(gn)
        ind = np.array([all_configs[b.configuration()] for b in bs])
        means = [b.analysis.mean.value for b in bs]
        p = ax.bar(ind + offset, means, width)
        offset += width

    pretty_label = lambda l: '{:,}'.format(int(l)).replace(',', ' ')

    def fmt_duration(value, _tick_number):
        if value < 0.001:
            return str(value*1000*1000) + u" µs"
        elif value < 1:
            return str(value*1000) + " ms"
        else:
            return str(value) + " s"
        return str(value)

    ind = np.arange(len(all_configs))
    ax.set_title('Benchmark results')
    ax.grid(True, ls='dashed', alpha=0.9)
    ax.set_yscale('log')
    ax.set_ylabel('Seconds')
    ax.yaxis.set_major_formatter(plt.FuncFormatter(fmt_duration))
    ax.set_xticks(ind + (offset - width) / 2)
    ax.set_xticklabels([pretty_label(l) for l in sorted(all_configs.keys())])
    ax.set_xlabel('Size of the set')
    ax.autoscale_view()

    ax.legend(groups)

    plt.show()

def plot_naive(bench):
    bgroups = [b for b in BGROUPS if b[0] in ['set', 'hashset', 'containers']]
    return plot(bench, bgroups)

def plot_ffi(bench):
    bgroups = [b for b in BGROUPS if b[0] in ['set', 'hashset', 'containers', 'ffi']]
    return plot(bench, bgroups)

def plot_native_div(bench):
    bgroups = [b for b in BGROUPS if b[0] in ['ffi', 'native-div']]
    return plot(bench, bgroups)

def plot_native_fast(bench):
    bgroups = [b for b in BGROUPS if b[0] in ['ffi', 'native-div', 'native', 'ghc']]
    return plot(bench, bgroups)

if __name__ == "__main__":
    try:
        to_plot = sys.argv[1]
    except IndexError:
        to_plot = 'summary'
    main(to_plot)
