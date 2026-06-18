#!/usr/bin/env python3
# plot_matches.py - overlay an ASP match file on its two images, side by side,
# for visual sanity-checking of interest point / disparity-derived matches.
# Each match gets a color from the LEFT point's position, and the SAME color is
# drawn on the right point. If the colors form a consistent spatial pattern in
# both panels, the matches correspond; scattered colors mean junk.
#
# Usage: plot_matches.py left.tif right.tif matches.match out.png [width] [maxpts]
#   width  preview width in pixels (default 700)
#   maxpts max points to draw (default 2000; dense match sets are subsampled)

import sys, os, subprocess, colorsys, tempfile, statistics, math
from PIL import Image, ImageDraw

PMF = os.path.expanduser('~/projects/StereoPipeline/install/bin/parse_match_file.py')

def orig_size(tif):
    out = subprocess.check_output(['gdalinfo', tif]).decode()
    for ln in out.splitlines():
        if ln.startswith('Size is'):
            w, h = ln.replace('Size is', '').split(',')
            return int(w), int(h)
    raise RuntimeError('no size for ' + tif)

def preview(tif, width):
    tmp = tempfile.mktemp(suffix='.png')
    subprocess.run(['gdal_translate', '-q', '-of', 'PNG', '-ot', 'Byte', '-scale',
                    '-outsize', str(width), '0', tif, tmp], check=True)
    return Image.open(tmp).convert('RGB'), tmp

def parse_matches(matchfile):
    # parse_match_file.py writes a header "nL nR", then nL left points, then nR
    # right points; each line is ONE point as "x y ix iy [scale] [interest] ...".
    # Pair left[i] with right[i] using the float x,y (columns 0,1).
    txt = tempfile.mktemp(suffix='.txt')
    subprocess.run(['python', PMF, matchfile, txt], check=True)
    with open(txt) as f:
        hdr = f.readline().split()
        nL, nR = int(hdr[0]), int(hdr[1])
        pts = []
        for line in f:
            p = line.split()
            if len(p) >= 2:
                pts.append((float(p[0]), float(p[1])))
    os.remove(txt)
    left, right = pts[:nL], pts[nL:nL + nR]
    n = min(len(left), len(right))
    return [(left[i][0], left[i][1], right[i][0], right[i][1]) for i in range(n)]

def main():
    left, right, match, out = sys.argv[1:5]
    width  = int(sys.argv[5]) if len(sys.argv) > 5 else 700
    maxpts = int(sys.argv[6]) if len(sys.argv) > 6 else 2000

    lw, lh = orig_size(left)
    rw, rh = orig_size(right)
    limg, lt = preview(left, width)
    rimg, rt = preview(right, width)
    lsx, lsy = limg.width / lw, limg.height / lh
    rsx, rsy = rimg.width / rw, rimg.height / rh

    matches = parse_matches(match)
    n = len(matches)
    step = max(1, n // maxpts)
    ld, rd = ImageDraw.Draw(limg), ImageDraw.Draw(rimg)
    for i in range(0, n, step):
        c1, r1, c2, r2 = matches[i]
        hue = (c1 / lw) % 1.0
        val = 0.5 + 0.5 * (r1 / lh)
        col = tuple(int(255 * x) for x in colorsys.hsv_to_rgb(hue, 1.0, val))
        x1, y1 = c1 * lsx, r1 * lsy
        x2, y2 = c2 * rsx, r2 * rsy
        ld.ellipse([x1-2, y1-2, x1+2, y1+2], outline=col, width=1)
        rd.ellipse([x2-2, y2-2, x2+2, y2+2], outline=col, width=1)

    gap = 10
    comp = Image.new('RGB', (limg.width + gap + rimg.width,
                             max(limg.height, rimg.height)), (255, 255, 255))
    comp.paste(limg, (0, 0))
    comp.paste(rimg, (limg.width + gap, 0))
    comp.save(out)

    dxs = [c2 - c1 for c1, r1, c2, r2 in matches]
    dys = [r2 - r1 for c1, r1, c2, r2 in matches]
    mdx, mdy = statistics.median(dxs), statistics.median(dys)
    print("%d matches; dx median %.2f dy median %.2f (pixels)" % (n, mdx, mdy))

    # Honest sub-pixel consistency: residual of each match to the robust (median)
    # translation. This is the ortho-pair analog of bundle_adjust reprojection
    # residuals - a median residual near ~0.1 px means tight matches; ~0.5 px
    # means the interest point matches are mediocre. (Assumes a translational
    # offset, which holds for co-registered orthos; rotation/scale would inflate
    # it - then fit an affine instead.)
    res = sorted(math.hypot(dx - mdx, dy - mdy) for dx, dy in zip(dxs, dys))
    med = statistics.median(res)
    rms = (sum(r * r for r in res) / len(res)) ** 0.5
    verdict = "tight" if med < 0.2 else ("ok" if med < 0.5 else "mediocre")
    print("residual to best-fit translation: median %.2f px, RMS %.2f px  [%s]"
          % (med, rms, verdict))
    print("saved " + out)
    os.remove(lt); os.remove(rt)

main()
