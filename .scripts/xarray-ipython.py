# Scripts to open files with Xarray.

import argparse
import logging

import numpy as np  # noqa: F401
import matplotlib.pyplot as plt  # noqa: F401
import xarray as xr


log = logging.getLogger()


def parse_args():
    parser = argparse.ArgumentParser(formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument('-files', nargs='*', type=str, action='append',
                        default=([]))
    dargs = vars(parser.parse_args())
    return dargs['files'][0]


def open_expand_dim(dim='time'):
    def exp(ds):
        return ds.expand_dims(dim)
    return xr.open_mfdataset(globals()['files'], preprocess=exp)


if __name__ == '__main__':
    files = parse_args()
    try:
        ds = xr.open_mfdataset(files)
        print(ds)
    except Exception as e:
        log.warning("%s", e.args[0])
        log.warning("Try 'open_expand_dim()'")
        log.warning("or 'xr.open_mfdataset(files, ...)'")
