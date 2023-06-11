import numpy as np
import csv
import matplotlib.pyplot as plt
import pandas as pd
import os
import sys
import autoreject
import mne


def make_clean_epochs(raw, l_freq=1, h_freq=None, duration=3, preload=True):
    raw.filter(l_freq=l_freq, h_freq=h_freq)
    epochs = mne.make_fixed_length_epochs(raw, duration=duration, preload=preload)
    ar = autoreject.AutoReject(
        n_interpolate=[1, 2, 3, 4], random_state=11, n_jobs=1, verbose=True
    )
    ar.fit(epochs)
    epochs_ar, reject_log = ar.transform(epochs, return_log=True)
    return epochs, epochs_ar, reject_log


def closest_smaller_index(events, X):
    array = events[:, 0]
    indices = np.where(array <= X)[0]
    if indices.size == 0:
        return int(0)
    else:
        idx = indices[np.argmax(array[indices])]
        type = events[idx, 2]
        return int(type)


def raw_to_eventsv(raw, duration):
    events = mne.events_from_annotations(raw)[0]
    eventsv = np.zeros(int(np.floor(len(raw) / 256 / duration)), dtype=np.int32)
    for i in range(len(eventsv)):
        eventsv[i] = closest_smaller_index(events, (i * duration * 256))
    return eventsv


def raw_to_eventsv_ar(raw, reject_log, duration):
    eventsv = raw_to_eventsv(raw, duration)
    eventsv_ar = eventsv[~reject_log.bad_epochs]
    return eventsv, eventsv_ar
