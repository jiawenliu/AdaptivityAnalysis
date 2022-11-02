# -*- coding: utf-8 -*-

import logging
import numpy as np
import os
import pickle
import time


def make_dirs(dir_path_list):
    """
    Creates a directory (if not already present) at the indicated path.
    :param dir_path_list: list of successive directories (e.g., dir_path_list[1] is created inside dir_path_list[0])
                          to be created
    :returns: path to the inner-most directory from the current directory, creating a directory whenever
              one doesn't exist on the path
    """

    n = len(dir_path_list)
    assert n > 0, "dir_path_list cannot be empty."

    save_path = None
    for i in range(n):
        if save_path is None:
            save_path = dir_path_list[i]
        else:
            save_path = os.path.join(save_path, dir_path_list[i])

        if not os.path.isdir(save_path):
            os.mkdir(save_path)

    return save_path


def create_file_for_logging(path, prefix="Exp_log_", name_str=None):
    """
    Creates a file for logging.
    :param path: path for log file
    :param prefix: prefix for log file
    :param name_str: name name_str that is appended to the prefix for naming the log file. If None, the current
                     timestamp is used to create a log file with a unique name.
    """
    if name_str is None:
        name_str = time.strftime("%Y%m%d-%H%M%S")
    logging.basicConfig(filename=os.path.join(path, prefix + name_str + ".log"), filemode='w',
                        level=logging.INFO)


def log_line(line, log=True):
    """
    Logs line in logfile if log=True.
    :param line: line to be logged
    :param log: if True, logs given line
    """
    if log:
        logging.info(line)


def write_to_file(filename, key, to_write_row, log=True):
    """
    Writes to_write_row in file filename for given key.
    :param filename: path to a .pickle file
    :param key: key for which to_write_row should be associated. key should be a tuple
    :param to_write_row: row to be written in file filename for given key
    :param log: if True, writes logs using function logline
    """

    if os.path.isfile(filename):
        with open(filename, 'rb') as handle:
            d = pickle.load(handle)
        d[key] = to_write_row
    else:
        d = {key: to_write_row}

    log_line("writing for key: " + str(key), log)
    with open(filename, 'wb') as handle:
        pickle.dump(d, handle)
    log_line("Updating file " + filename.split('\\')[-1] + " done.", log)


def get_from_file(filename, key, log=True):
    """
    Gets entry for key (if exists) in file filename.
    :param filename: path to a .pickle file
    :param key: key for which value needs to be fetched. key should be a tuple
    :param log: if True, writes logs using function logline
    :returns: value in file filename for given key (if exists), else None
    """

    val = None
    log_line("getting for key: " + str(key), log)
    if os.path.isfile(filename):
        with open(filename, 'rb') as handle:
            d = pickle.load(handle)
            if key in d:
                log_line("Found in file.", log)
                val = d[key]
            else:
                log_line("Not found.", log)
    else:
        log_line("File not found.", log)
    return val


def initialize_with_str_seed(init_str):
    """
    Initializes random number generator with seed corresponding to given input string init_str.
    :param init_str: Initialization string according to which seed will be computed. Seed is the sum of the ASCII
                     values of each character in init_str.
    """
    rnd_val = 0
    if init_str:
        for c in init_str:
            rnd_val += ord(c)
    np.random.seed(rnd_val)
