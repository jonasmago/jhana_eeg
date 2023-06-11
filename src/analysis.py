import numpy as np
import scipy.stats as stats
import matplotlib.pyplot as plt
import mne


def plot_compare_psd(*epochs, alpha=0.05, labels=None, fill=True):
    plt.figure(figsize=(5, 3), dpi=800)

    if labels is None:
        labels = [f"cond_{i}" for i in range(len(epochs))]

    psds_all = []
    for c, label in zip(epochs, labels):
        psds, freqs = compute_psd(
            c, fmin=1.0, fmax=40, sfreq=256, n_fft=1280, average=None
        )
        psds_all.append(psds)
        # Log transform your data and mean over measures and channels
        log_all = np.mean(np.log10(psds), axis=(1, 3))
        # Mean and standard deviation over samples
        log_mean = np.mean(log_all, axis=0)
        log_sd = np.std(log_all, axis=0)

        plt.plot(freqs, log_mean, label=label)
        if fill:
            plt.fill_between(freqs, log_mean - log_sd, log_mean + log_sd, alpha=0.2)

    # Add markers for significant frequencies
    p_values = compute_p_values(psds_all)
    significant_freqs = freqs[p_values < alpha]
    top_of_plot = plt.gca().get_ylim()[1]  # gets the current top of the plot
    #    plt.scatter(significant_freqs, [top_of_plot]*len(significant_freqs), color='r')
    plt.scatter(
        significant_freqs,
        [top_of_plot] * len(significant_freqs),
        color="r",
        s=2,
        alpha=0.5,
    )

    plt.legend()
    plt.show(block=False)

    # Assuming 'save_dir' is defined somewhere else in your code
    # os.chdir(save_dir)
    # plt.savefig("/Users/jonasmago/Desktop/xxx", dpi=600, bbox_inches='tight', transparent=True)


def compute_p_values(psds_all):
    # Compute mean over channels and measures for each frequency
    means = [np.mean(psds, axis=(1, 3)) for psds in psds_all]

    # Compute t-test for each frequency. We use axis=0 to compare the groups for each frequency
    _, p_values = stats.f_oneway(*means)

    return p_values


def compute_psd(epochs, fmin=1.0, fmax=40, sfreq=256, n_fft=1280, average=None):
    psds, freqs = mne.time_frequency.psd_array_welch(
        epochs._get_data(),
        sfreq=sfreq,
        n_fft=n_fft,
        fmin=fmin,
        fmax=fmax,
        average=average,
    )
    return psds, freqs


def plot_spectogram(epochs, eventsv, out_path=None, title=None, fmin=1.0, fmax=40):
    ch_names = epochs.info.ch_names
    psds, freqs = compute_psd(
        epochs, fmin=fmin, fmax=fmax, sfreq=256, n_fft=1280, average=None
    )

    data = np.average(psds, axis=(2, 3))
    normalized_data = data / data[0, :]
    fig, ax1 = plt.subplots(figsize=(10, 6))

    # plot the normalized data
    im = ax1.imshow(normalized_data.T, aspect="auto", cmap="viridis", vmin=-5, vmax=5)
    ax1.set_xlabel("Columns")

    # set the y-axis labels
    ax1.set_yticks(np.arange(len(ch_names)))
    ax1.set_yticklabels(ch_names)

    # find indices where the vector changes
    change_indices = (
        np.where(np.diff(eventsv) > 0)[0] + 1
    )  # '+1' because diff() reduces the array size by 1

    # add vertical lines and annotations
    for idx in change_indices:
        ax1.axvline(x=idx, color="r", linestyle="--")
        ax1.text(idx, -2, int(eventsv[idx]), color="red", ha="center", va="bottom")

    plt.colorbar(im, label="Value in cell")  # optional, adds a colorbar on the right

    plt.tight_layout()
    plt.title(title)
    if out_path:
        plt.savefig(out_path)
    else:
        plt.show()


def import_epochs(fname, all_epochs=False):
    if all_epochs:
        epochs_mindfulness = mne.read_epochs(f"{fname[0][:-11]}all-epo.fif")
    else:
        epochs_ar = mne.read_epochs(f"{fname[:-11]}ar-epo.fif")
    eventsv = np.load(f"{fname[:-11]}eventsv.npy")
    eventsv_ar = np.load(f"{fname[:-11]}eventsv_ar.npy")
    reject_log = np.load(f"{fname[:-11]}reject_log.npz")
    return epochs_ar, eventsv, eventsv_ar, reject_log
