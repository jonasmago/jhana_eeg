import numpy as np
import scipy.stats as stats
import matplotlib.pyplot as plt
import mne
import networkx as nx
import pandas as pd
import seaborn as sns
from mne_connectivity import envelope_correlation

# seaborn


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


def compute_connectome(epochs):
    connectome = envelope_correlation(epochs).get_data()
    return connectome


def plot_connectomes(corr_matrices, sub_titles):
    # Ensure corr_matrices and sub_titles are lists for consistent handling
    if not isinstance(corr_matrices, list):
        corr_matrices = [corr_matrices]
        color_lims = np.percentile(np.array(corr_matrices), [5, 95])
    else:
        color_lims = np.percentile(np.array(corr_matrices[0]), [5, 95])
    color_lims = [0, 0.25]
    if not isinstance(sub_titles, list):
        sub_titles = [sub_titles]

    fig, axes = plt.subplots(nrows=1, ncols=len(corr_matrices))
    fig.suptitle("Correlation Matrices")

    # If there's only one subplot, axes is not a list, so wrap it in a list for consistent handling
    if len(corr_matrices) == 1:
        axes = [axes]

    for ci, corr_matrix in enumerate(corr_matrices):
        if len(corr_matrix.shape) == 4:
            corr_matrix = np.average(corr_matrix, axis=0)
        ax = axes[ci]
        mpbl = ax.imshow(corr_matrix, clim=color_lims)
        ax.set_xlabel(sub_titles[ci])

    fig.subplots_adjust(right=0.8)
    cax = fig.add_axes([0.85, 0.2, 0.025, 0.6])
    cbar = fig.colorbar(axes[0].images[0], cax=cax)
    cbar.set_label("Correlation Coefficient")


def compute_graph_summary(connectome_matrix):
    if len(connectome_matrix.shape) == 3:
        connectome_matrix = connectome_matrix[:, :, 0]
    if len(connectome_matrix.shape) == 4:
        connectome_matrix = np.average(connectome_matrix, axis=0)[:, :, 0]

        # Create weighted graph from adjacency matrix
    G = nx.from_numpy_array(connectome_matrix)

    # Compute the average shortest path length for the weighted graph
    avg_shortest_path_length = nx.average_shortest_path_length(G, weight="weight")

    # Compute the node strengths (weighted degree) and their summary statistics
    strengths = [deg for node, deg in G.degree(weight="weight")]
    avg_strength = np.mean(strengths)
    std_strength = np.std(strengths)

    # Compute the betweenness centrality for weighted graph and its summary statistics
    betweenness_centrality = nx.betweenness_centrality(G, weight="weight")
    avg_betweenness = np.mean(list(betweenness_centrality.values()))
    std_betweenness = np.std(list(betweenness_centrality.values()))

    # Compute the closeness centrality for weighted graph and its summary statistics
    closeness_centrality = nx.closeness_centrality(G, distance="weight")
    avg_closeness = np.mean(list(closeness_centrality.values()))
    std_closeness = np.std(list(closeness_centrality.values()))

    # Compute the clustering coefficient for weighted graph and its summary statistics
    clustering_coefficient = nx.clustering(G, weight="weight")
    avg_clustering = np.mean(list(clustering_coefficient.values()))
    std_clustering = np.std(list(clustering_coefficient.values()))

    # Store all statistics in a dictionary
    graph_summary = {
        "avg_shortest_path_length": avg_shortest_path_length,
        "avg_strength": avg_strength,
        "std_strength": std_strength,
        "avg_betweenness": avg_betweenness,
        "std_betweenness": std_betweenness,
        "avg_closeness": avg_closeness,
        "std_closeness": std_closeness,
        "avg_clustering": avg_clustering,
        "std_clustering": std_clustering,
    }

    return graph_summary


def print_graph_summary(graph_summary):
    for measure, value in graph_summary.items():
        print(f"{measure}: {value:.2f}")


def compute_group_summary(connectome_matrices_4d):
    # This function computes the summary for a group of connectomes
    group_summary = []
    # Iterate over the first dimension
    for i in range(connectome_matrices_4d.shape[0]):
        # Extract each 2D connectome matrix and ignore the extra dimension
        connectome_matrix = connectome_matrices_4d[
            i, :, :, 0
        ]  # or use np.squeeze(connectome_matrices_4d[i])
        graph_summary = compute_graph_summary(connectome_matrix)
        group_summary.append(graph_summary)
    return group_summary


def compare_groups(group_one_matrices_4d, group_two_matrices_4d, measures):
    # Compute summary for both groups
    group_one_summary = compute_group_summary(group_one_matrices_4d)
    group_two_summary = compute_group_summary(group_two_matrices_4d)

    # Combine the data from both groups into a single dataframe for visualization
    group_one_df = pd.DataFrame(group_one_summary)
    group_one_df["group"] = "Group One"
    group_two_df = pd.DataFrame(group_two_summary)
    group_two_df["group"] = "Group Two"
    data = pd.concat([group_one_df, group_two_df])

    # Create a subplot for each measure
    fig, axes = plt.subplots(
        nrows=len(measures), figsize=(10, len(measures) * 5), dpi=200
    )
    fig.suptitle("Comparison of measures between groups")

    for i, measure in enumerate(measures):
        ax = axes[i]
        # Perform t-tests for each measure
        group_one_measure = [summary[measure] for summary in group_one_summary]
        group_two_measure = [summary[measure] for summary in group_two_summary]
        t_stat, p_value = stats.ttest_ind(group_two_measure, group_one_measure)

        # Calculate mean values
        mean_group_one = np.mean(group_one_measure)
        mean_group_two = np.mean(group_two_measure)

        # Create a violin plot for the current measure
        sns.violinplot(x="group", y=measure, data=data, ax=ax)
        ax.set_title(f"{measure}: t={t_stat:.2f}, p={p_value:.2f}")

        # Display mean values
        ax.text(
            0,
            mean_group_one,
            f"Mean: {mean_group_one:.2f}",
            color="white",
            ha="center",
            va="center",
            fontsize=12,
        )
        ax.text(
            1,
            mean_group_two,
            f"Mean: {mean_group_two:.2f}",
            color="white",
            ha="center",
            va="center",
            fontsize=12,
        )

        # If the p-value is less than 0.05, indicate it on the plot
        if p_value < 0.05:
            ax.text(
                0.5,
                0.9,
                "*",
                transform=ax.transAxes,
                ha="center",
                va="center",
                fontsize=20,
            )

    plt.tight_layout(
        rect=[0, 0, 1, 0.96]
    )  # Adjust the layout to make sure the title is visible
    plt.show()
