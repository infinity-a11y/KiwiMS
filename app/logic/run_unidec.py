import sys
import unidec
import re

# Initialize UniDec engine
engine = unidec.UniDec()

# Convert Waters .raw to txt
input_file = sys.argv[1]
engine.raw_process(input_file)

# Open converted txt file
txt_file = re.sub(r"\.raw$", "_rawdata.txt", input_file)
engine.open_file(txt_file)

# Get deconvolution parameters
def to_number(value):
    try:
        return float(value) if value.strip() else ""
    except ValueError:
        return ""

engine.config.startz = to_number(sys.argv[2])
engine.config.endz = to_number(sys.argv[3])
engine.config.minmz = to_number(sys.argv[4])
engine.config.maxmz = to_number(sys.argv[5])
engine.config.masslb = to_number(sys.argv[6])
engine.config.massub = to_number(sys.argv[7])
engine.config.massbins = to_number(sys.argv[8])
engine.config.peakthresh = to_number(sys.argv[9])
engine.config.peakwindow = to_number(sys.argv[10])
engine.config.peaknorm = to_number(sys.argv[11])
engine.config.time_start = to_number(sys.argv[12])
engine.config.time_end = to_number(sys.argv[13])

# Process and deconvolve the data
engine.process_data()
engine.run_unidec()
engine.pick_peaks()
