#!/usr/bin/env python3
"""Measure Halo pills against their windows in a harness screenshot.

Reads a binary PPM (`headless.sh shot out.ppm`). Windows are found by their
client background colour, so open clients with a known one, e.g.
`foot -o colors.background=1e3a5f`. The pill is the glass-coloured blob in the
band above each window: the desktop colour seen through `glass_glance`.

  halo-measure.py SHOT.ppm --scale 1.5 --mode dark --clear 2a2a2e \
      --window 1e3a5f [--window 3a1e5f ...] [--usable-top 0] [--check]

Prints one JSON object per window. With --check it exits non-zero unless every
measured window has the design's numbers (within a pixel of rounding): pill
32 x scale tall, its bottom 4 x scale above the window, no wider than the window,
and the window at least 40 x scale below the usable area's top.
"""

import argparse
import json
import math
import sys


def read_ppm(path):
    with open(path, "rb") as f:
        data = f.read()
    fields = []
    pos = 0
    while len(fields) < 4:
        while data[pos : pos + 1].isspace():
            pos += 1
        if data[pos : pos + 1] == b"#":
            pos = data.index(b"\n", pos) + 1
            continue
        end = pos
        while not data[end : end + 1].isspace():
            end += 1
        fields.append(data[pos:end])
        pos = end
    if fields[0] != b"P6" or int(fields[3]) != 255:
        raise SystemExit(f"{path}: not an 8-bit binary PPM")
    width, height = int(fields[1]), int(fields[2])
    return width, height, data[pos + 1 :]


def hex_rgb(value):
    value = value.lstrip("#")
    return tuple(int(value[i : i + 2], 16) for i in (0, 2, 4))


def dist(a, b):
    return sum(abs(x - y) for x, y in zip(a, b))


class Image:
    def __init__(self, path):
        self.width, self.height, self.pixels = read_ppm(path)

    def at(self, x, y):
        i = (y * self.width + x) * 3
        return tuple(self.pixels[i : i + 3])


def find_window(img, color, tol=24):
    """The client's box: where its background colour is, refined against the
    colour actually drawn there (clients and gamma shift it a little). The pill's
    shadow falls on the client's first rows, so the match is loose."""
    rows, cols, seen = {}, {}, {}
    step = 2
    for y in range(0, img.height, step):
        for x in range(0, img.width, step):
            p = img.at(x, y)
            if dist(p, color) <= tol:
                rows[y] = rows.get(y, 0) + 1
                cols[x] = cols.get(x, 0) + 1
                seen[p] = seen.get(p, 0) + 1
    if not rows:
        return None
    # Lines where the colour fills a good share, so stray pixels of the same
    # colour elsewhere (antialiased text in the pill) do not stretch the box.
    ys = [y for y, n in rows.items() if n >= 0.3 * max(rows.values())]
    xs = [x for x, n in cols.items() if n >= 0.1 * max(cols.values())]
    left, right, top, bottom = min(xs), max(xs), min(ys), max(ys)
    drawn = max(seen, key=seen.get)
    near = lambda x, y: 0 <= x < img.width and 0 <= y < img.height and dist(img.at(x, y), drawn) <= tol
    # Refine each edge within a few pixels of the coarse box, along many lines,
    # so text along an edge or another window over part of it does not move it.
    cols = [left + (right - left) * i // 40 for i in range(4, 37)]
    rows = [top + (bottom - top) * i // 40 for i in range(4, 37)]
    # An edge is the first line, within a few pixels of the coarse box, where any
    # of those samples shows the background: text never fills a whole line.
    row = lambda y: any(near(x, y) for x in cols)
    col = lambda x: any(near(x, y) for y in rows)
    top = next((y for y in range(top - 3, top + 4) if row(y)), top)
    # An unfocused window's 1px border is close to its background. A top line
    # brighter than the line below it is that border: shadows only darken.
    bright = lambda y: sum(sum(img.at(x, y)) for x in cols) / len(cols)
    for _ in range(2):
        if top + 1 < img.height and bright(top) > bright(top + 1) + 15:
            top += 1
    bottom = next((y for y in range(bottom + 3, bottom - 4, -1) if row(y)), bottom)
    left = next((x for x in range(left - 3, left + 4) if col(x)), left)
    right = next((x for x in range(right + 3, right - 4, -1) if col(x)), right)
    return left, top, right + 1, bottom + 1


def glass(clear, mode):
    """`--glass-glance` over the desktop: black .76 dark, white .76 light."""
    over = (0, 0, 0) if mode == "dark" else (255, 255, 255)
    return tuple(round(0.76 * o + 0.24 * c) for o, c in zip(over, clear))


def is_glass(p, behind, mode):
    """Glass darkens what is behind it in dark mode and lightens it in light mode;
    the pill's shadow only ever darkens, and by at most a quarter."""
    if mode == "dark":
        return sum(p) < 0.6 * sum(behind)
    return sum(p) > sum(behind) + 6


def widest_run(xs, gap):
    """The largest cluster of sorted x positions no more than `gap` apart: (lo, hi, count)."""
    best = cur = (xs[0], xs[0], 1)
    for x in xs[1:]:
        cur = (cur[0], x, cur[2] + 1) if x - cur[1] <= gap else (x, x, 1)
        if cur[2] > best[2]:
            best = cur
    return best


def find_pill(img, win, clear, mode, scale):
    """The glass run of rows nearest above `win`: (left, top, right, bottom).

    Rows count as pill while they hold glass pixels within the window's span;
    text and icons inside the pill do not break a row. The run closest to the
    window is the window's own pill, not another window's above it.
    """
    left, top, right, _ = win
    expected = glass(clear, mode)
    band_top = max(0, top - round(60 * scale))
    x0, x1 = max(0, left - 2), min(img.width, right + 2)
    runs = {}
    for y in range(band_top, top):
        # Light glass is only a little lighter than the desktop, and window shadows
        # darken both: compare with the desktop beside the pill on the same row.
        # The brightest of a few pixels out, so a frame edge's antialiasing is not taken for desktop.
        behind = max((img.at(max(0, x0 - k), y) for k in range(0, round(8 * scale) + 1, 2)), key=sum)
        if mode == "dark" or dist(behind, clear) > 40:
            behind = clear
        hits = [x for x in range(x0, x1) if is_glass(img.at(x, y), behind, mode)]
        if hits:
            runs[y] = widest_run(hits, round(20 * scale))
    if not runs:
        return None
    # The pill is the widest glass in the band; a row belongs to it only where
    # its glass overlaps that, so a lit frame edge or a line of text beside the
    # pill (a fullscreen prompt) does not stretch it.
    ref = max(runs.values(), key=lambda r: r[2])
    rows = {y: (lo, hi + 1) for y, (lo, hi, n) in runs.items()
            if n >= round(24 * scale) and lo <= ref[1] and hi >= ref[0]}
    if not rows:
        return None
    bottom = max(rows) + 1
    first = bottom - 1
    while first - 1 in rows:
        first -= 1
    span = [rows[y] for y in range(first, bottom)]
    pl, pr = min(s[0] for s in span), max(s[1] for s in span)
    # The 0.5px edge is not glass: take in the row or two it tints.
    mid = (pl + pr) // 2
    edge = math.ceil(0.5 * scale)
    for _ in range(edge):
        if first > band_top and dist(img.at(mid, first - 1), clear) > 30:
            first -= 1
    for _ in range(edge):
        below = img.at(mid, bottom)
        if bottom < top and dist(below, clear) > 30 and dist(below, expected) > 30:
            bottom += 1
    return pl, first, pr, bottom


def frame_top(img, win, scale):
    """The client box starts inside the 1px frame; the window's edge is above it."""
    return win[1] - round(scale)


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("shot")
    parser.add_argument("--scale", type=float, default=1.0)
    parser.add_argument("--mode", choices=["dark", "light"], default="dark")
    parser.add_argument("--clear", default="2a2a2e")
    parser.add_argument("--window", action="append", default=[], help="client background, RRGGBB[:tolerance]")
    parser.add_argument("--usable-top", type=int, default=0, help="usable area top, pixels")
    parser.add_argument("--check", action="store_true")
    parser.add_argument(
        "--fullscreen", action="store_true", help="the pill hangs inside the window's top edge"
    )
    args = parser.parse_args()

    img = Image(args.shot)
    clear = hex_rgb(args.clear)
    s = args.scale
    failures = []
    for color in args.window:
        hex_color, _, tol = color.partition(":")
        win = find_window(img, hex_rgb(hex_color), int(tol or 24))
        if win is None:
            print(json.dumps({"window": color, "found": False}))
            failures.append(f"{color}: no window")
            continue
        top = frame_top(img, win, s)
        width = win[2] - win[0] + 2 * round(s)
        if args.fullscreen:
            # Fullscreen: the pill hangs 10px inside the top edge, over the client.
            behind = hex_rgb(color)
            pill = find_pill(img, (win[0], round(60 * s), win[2], win[3]), behind, args.mode, s)
        else:
            pill = find_pill(img, (win[0], top, win[2], win[3]), clear, args.mode, s)
        report = {
            "window": color,
            "rect": [win[0] - round(s), top, width, win[3] - top + round(s)],
            "window_top_from_usable": top - args.usable_top,
            "logical_width": round(width / s),
        }
        if pill is None:
            report["pill"] = None
        elif args.fullscreen:
            report["pill"] = [pill[0], pill[1], pill[2] - pill[0], pill[3] - pill[1]]
            report["pill_height"] = pill[3] - pill[1]
            report["pill_top"] = pill[1]
        else:
            report["pill"] = [pill[0], pill[1], pill[2] - pill[0], pill[3] - pill[1]]
            report["pill_height"] = pill[3] - pill[1]
            report["gap"] = top - pill[3]
            report["pill_center_offset"] = (pill[0] + pill[2]) / 2 - (win[0] + win[2]) / 2
        if args.fullscreen:
            report["expected"] = {"pill_height": 32 * s, "pill_top": 10 * s}
            print(json.dumps(report))
            if args.check and (pill is None or abs(pill[1] - 10 * s) > 1.5):
                failures.append(f"{color}: fullscreen pill not 10px inside the top")
            continue
        report["expected"] = {
            "pill_height": 32 * s,
            "gap": 4 * s,
            "window_top_from_usable_min": 40 * s,
        }
        print(json.dumps(report))
        if args.check:
            name = color
            if pill is None:
                failures.append(f"{name}: no pill above the window")
                continue
            tol = 1.5
            if abs(report["pill_height"] - 32 * s) > tol:
                failures.append(f"{name}: pill {report['pill_height']}px tall, want {32 * s}")
            if abs(report["gap"] - 4 * s) > tol:
                failures.append(f"{name}: gap {report['gap']}px, want {4 * s}")
            if pill[2] - pill[0] > width + 1:
                failures.append(f"{name}: pill wider than the window")
            if report["window_top_from_usable"] < 40 * s - tol:
                failures.append(
                    f"{name}: window top {report['window_top_from_usable']}px below the usable top, want >= {40 * s}"
                )
    for failure in failures:
        print("FAIL", failure, file=sys.stderr)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
