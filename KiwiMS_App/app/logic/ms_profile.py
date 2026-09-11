"""Profile a mass-spectrometry acquisition and suggest deconvolution parameters.

Run as a CLI so the Shiny process never has to host a Python interpreter:

    python ms_profile.py <sample-path> [<sample-path> ...]

Prints one JSON document on stdout: {"ok": bool, "samples": [...], "error": str}.
Diagnostics from the vendor readers go to stderr, so stdout stays parseable.

Why the parameters can be guessed at all
----------------------------------------
An intact protein under electrospray does not give one peak, it gives a ladder
of charge states. Two neighbouring peaks of that ladder, at m/z values lo < hi
carrying charges z+1 and z, fix both unknowns:

    M = z (hi - proton) = (z + 1)(lo - proton)   =>   z = (lo - proton)/(hi - lo)

So every adjacent pair of peaks votes for a charge and therefore a mass. Real
charge ladders produce many pairs that agree to within a few parts in 10^5;
noise, adducts and co-eluting junk produce pairs that agree with nothing. The
count of agreeing pairs is the confidence measure this module reports, and it
is what separates "we know the mass" from "use wide defaults".

That same count is also how the elution window is chosen. The obvious rule --
take the biggest peak in the chromatogram -- picks the solvent front or a salt
cluster often enough to be useless: in the reference corpus the largest TIC peak
of one sample yields no ladder at all, while a peak a third its height yields a
clean 13-pair envelope at 70.2 kDa. So every candidate window is scored by the
envelope it produces and the best-corroborated one wins.
"""
import json
import os
import sys
import time

import numpy as np
from scipy.signal import find_peaks

# Vendor reader diagnostics are chatty and unconditional; keep stdout clean.
_real_stdout = sys.stdout
sys.stdout = sys.stderr

from unidec import tools as ud  # noqa: E402

PROTON = 1.007276467
MIN_PAIRS = 3           # fewer agreeing pairs than this is not an envelope
MAX_REL_SD = 0.01       # agreement required among the surviving pairs
HIGH_CONF_PAIRS = 6     # at or above this, the mass estimate drives the ranges


def _charge_envelope(spec, max_peaks=25):
    """Neutral mass and charge range implied by the charge-state ladder."""
    mz, inten = spec[:, 0], spec[:, 1].astype(float)
    if len(mz) < 20 or inten.max() <= 0:
        return None
    top = inten.max()

    # Threshold against the noise floor, not against the tallest peak in the
    # spectrum. A screening sample carries the compound as well as the protein,
    # and a small molecule ionises far better: in the reference corpus the
    # compound peaks at m/z 507 and 604 are ~40x the protein's, so a threshold
    # set at 2% of the maximum hid the entire charge ladder and those samples
    # profiled as "no envelope". A median+MAD floor is blind to how tall the
    # brightest peak happens to be.
    med = float(np.median(inten))
    mad = float(np.median(np.abs(inten - med)))
    noise = med + 5.0 * 1.4826 * mad if mad > 0 else 0.0
    height = max(noise, 0.001 * top)

    # Peaks nearer than ~0.2% of the axis are isotope structure, not a new
    # charge state.
    dist = max(1, int(len(mz) * 0.002))
    idx, _ = find_peaks(inten, height=height, distance=dist,
                        prominence=height)
    if len(idx) < 2:
        return None
    # Cap at the strongest few. Feeding in many more weak peaks measurably
    # degrades the estimate rather than improving it: on the reference Thermo
    # file a cap of 60 lets the voting settle on 38,653 Da, exactly twice the
    # true 19,326 Da, because pairing every *second* charge state also yields a
    # self-consistent ladder at half the charge. The strongest peaks of a real
    # envelope are adjacent, so a tight cap keeps that aliasing out.
    order = idx[np.argsort(inten[idx])[::-1]][:max_peaks]
    pk = np.sort(mz[order])

    ests = []
    for lo, hi in zip(pk[:-1], pk[1:]):
        gap = hi - lo
        if gap <= 0:
            continue
        zf = (lo - PROTON) / gap
        z = int(round(zf))
        # A real neighbouring pair implies a near-integer charge; anything else
        # is two unrelated peaks that happen to sit side by side.
        if z < 2 or z > 100 or abs(zf - z) > 0.2:
            continue
        m = z * (hi - PROTON)
        if 500 < m < 500000:
            ests.append((m, z, lo, hi))
    if len(ests) < MIN_PAIRS:
        return None

    masses = np.array([e[0] for e in ests])
    med = float(np.median(masses))
    keep = [e for e in ests if abs(e[0] - med) / med < 0.02]
    if len(keep) < MIN_PAIRS:
        return None

    km = np.array([e[0] for e in keep])
    kz = np.array([e[1] for e in keep])
    kmz = np.concatenate([[e[2] for e in keep], [e[3] for e in keep]])
    mass = float(np.median(km))
    rel_sd = float(np.std(km) / mass) if mass else 1.0
    if rel_sd > MAX_REL_SD:
        return None

    return {
        "mass": mass,
        "rel_sd": rel_sd,
        "n_pairs": int(len(keep)),
        "zmin": int(kz.min()),
        "zmax": int(kz.max() + 1),
        "mz_lo": float(kmz.min()),
        "mz_hi": float(kmz.max()),
    }


def _candidate_windows(tic, n=6, frac=0.15):
    """Elution regions around the strongest chromatographic peaks."""
    t, y = tic[:, 0], tic[:, 1].astype(float)
    if len(t) < 5:
        return [(float(t.min()), float(t.max()))]
    base = np.percentile(y, 10)
    yc = np.clip(y - base, 0, None)
    if yc.max() <= 0:
        return [(float(t.min()), float(t.max()))]

    idx, _ = find_peaks(yc, height=0.02 * yc.max(),
                        distance=max(1, len(t) // 50))
    if len(idx) == 0:
        idx = np.array([int(np.argmax(yc))])
    idx = idx[np.argsort(yc[idx])[::-1]][:n]

    wins = []
    for i in idx:
        thr = frac * yc[i]
        lo = hi = int(i)
        while lo > 0 and yc[lo - 1] >= thr:
            lo -= 1
        while hi < len(t) - 1 and yc[hi + 1] >= thr:
            hi += 1
        w = (round(float(t[lo]), 3), round(float(t[hi]), 3))
        if w[1] > w[0] and w not in wins:
            wins.append(w)
    return wins or [(float(t.min()), float(t.max()))]


def _signal_mz_range(spec, frac=0.005):
    if spec.size == 0:
        return 0.0, 0.0
    mz, inten = spec[:, 0], spec[:, 1].astype(float)
    on = mz[inten >= frac * inten.max()] if inten.max() > 0 else mz
    if len(on) == 0:
        return float(mz.min()), float(mz.max())
    return float(on.min()), float(on.max())


def profile_one(path):
    t0 = time.perf_counter()
    out = {"path": path, "name": os.path.basename(path)}

    d = ud.get_importer(path)
    if d is None:
        raise IOError("No reader for this file")
    out["importer"] = type(d).__name__
    try:
        out["polarity"] = d.get_polarity()
    except Exception:
        out["polarity"] = None

    tic = np.asarray(d.get_tic())
    out["run_start"] = round(float(tic[:, 0].min()), 3)
    out["run_end"] = round(float(tic[:, 0].max()), 3)

    scored = []
    for w in _candidate_windows(tic):
        try:
            spec = np.asarray(d.get_data(time_range=w))
        except Exception:
            continue
        # A window narrower than the scan spacing can come back empty.
        if spec.size == 0 or spec.ndim != 2 or spec.shape[0] < 20:
            continue
        lo, hi = _signal_mz_range(spec)
        scored.append({
            "window": [float(w[0]), float(w[1])],
            "env": _charge_envelope(spec),
            "mz_lo": round(lo, 2),
            "mz_hi": round(hi, 2),
            "mz_bin": round(float(np.median(np.diff(spec[:, 0]))), 5),
        })

    if not scored:
        raise IOError("No usable spectra anywhere in this acquisition")

    valid = [s for s in scored if s["env"]]
    # Most corroborating pairs wins; tightest agreement breaks ties.
    valid.sort(key=lambda s: (-s["env"]["n_pairs"], s["env"]["rel_sd"]))
    best = valid[0] if valid else scored[0]

    out["windows_tried"] = len(scored)
    out["windows_valid"] = len(valid)
    out["window"] = best["window"]
    out["mz_lo"], out["mz_hi"] = best["mz_lo"], best["mz_hi"]
    out["mz_bin"] = best["mz_bin"]
    out["envelope"] = best["env"]
    out["alternatives"] = [
        {"window": s["window"],
         "mass": round(s["env"]["mass"], 1),
         "n_pairs": s["env"]["n_pairs"]}
        for s in valid[1:4]
    ]
    out["elapsed_s"] = round(time.perf_counter() - t0, 2)
    out.update(_suggest(out))
    return out


def _round_to(x, step):
    return float(round(x / step) * step)


def _suggest(p):
    """Concrete UniDec parameters implied by the profile."""
    env = p.get("envelope")
    s = {
        "time_start": p["window"][0],
        "time_end": p["window"][1],
        "minmz": round(max(0.0, p["mz_lo"] - 20), 1),
        "maxmz": round(p["mz_hi"] + 20, 1),
        "confidence": "low",
    }
    # Mass bin: no point resolving finer than the raw m/z sampling, and 1 Da is
    # the sane floor for intact protein work.
    s["massbins"] = 1.0 if p.get("mz_bin", 0) >= 0.005 else 0.5

    if env:
        m = env["mass"]
        # +/-25% brackets glycoforms, truncations and adduct series without
        # letting the mass axis grow so wide that the deconvolution crawls.
        pad = max(0.25 * m, 2000.0)
        s["masslb"] = _round_to(max(500.0, m - pad), 100)
        s["massub"] = _round_to(m + pad, 100)
        # Five charge states of headroom either side of what was actually seen.
        s["startz"] = max(1, env["zmin"] - 5)
        s["endz"] = env["zmax"] + 5
        s["predicted_mass"] = round(m, 1)
        s["n_pairs"] = env["n_pairs"]
        s["confidence"] = "high" if env["n_pairs"] >= HIGH_CONF_PAIRS else "medium"
    else:
        # Nothing resolvable: keep the ranges wide rather than guess narrow.
        s["masslb"], s["massub"] = 5000.0, 100000.0
        s["startz"], s["endz"] = 1, 50
    return s


def main(argv):
    samples, errors = [], []
    for path in argv:
        try:
            samples.append(profile_one(path))
        except Exception as exc:
            errors.append({"path": path, "name": os.path.basename(path),
                           "error": "%s: %s" % (type(exc).__name__, exc)})
    doc = {"ok": bool(samples), "samples": samples, "errors": errors}
    if not samples:
        doc["error"] = errors[0]["error"] if errors else "No samples profiled"
    _real_stdout.write(json.dumps(doc))
    _real_stdout.flush()
    return 0 if samples else 1


if __name__ == "__main__":
    if len(sys.argv) < 2:
        _real_stdout.write(json.dumps(
            {"ok": False, "samples": [], "errors": [],
             "error": "usage: ms_profile.py <sample-path> ..."}))
        sys.exit(2)
    sys.exit(main(sys.argv[1:]))
