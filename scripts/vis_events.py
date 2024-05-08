import glob
import mne

template = "../data/mne_raw/*control-mindfulness_mmn-raw_phot-events.fif"
# template = "../data/mne_raw/*jhana_mmn-raw_phot-events.fif"
# template = "../data/mne_raw/*mindfulness_mmn-raw_phot-events.fif"


"""
LTP_post-baseline_ltp-raw_phot-events.fif
LTP_post-early_ltp-raw_phot-events.fif
LTP_post-late_ltp-raw_phot-events.fif
LTP_post-rmmn-raw_phot-events.fif
LTP_post-tetanus-raw_phot-events.fif


LTP_pre-baseline_ltp-raw_phot-events.fif
LTP_pre-early_ltp-raw_phot-events.fif
LTP_pre-late_ltp-raw_phot-events.fif
LTP_pre-rmmn-raw_button-events.fif
LTP_pre-rmmn-raw_phot-events_button-events.fif
LTP_pre-rmmn-raw_phot-events.fif
LTP_pre-tetanus-raw_phot-events.fif
"""


files = glob.glob(template)
for file in files:
    raw = mne.io.read_raw_fif(file)
    raw.plot(block=True)
