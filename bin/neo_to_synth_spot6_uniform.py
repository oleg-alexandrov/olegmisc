#!/usr/bin/env python3

"""Convert a Pleiades Neo DIM XML into a synthetic SPOT6 DIM XML with
uniform timestamps.

Same as neo_to_synth_spot6.py but additionally resamples the ephemeris
and quaternion timestamps onto a truly uniform time grid using Lagrange
interpolation. This ensures the SPOT6 code path (which uses a
first-interval delta_t) produces the same camera model as the Neo code
path (which uses an average delta_t).

Usage:
  python neo_to_synth_spot6_uniform.py <neo_dim.xml> <spot6_template.xml> <output.xml>
"""

import sys
import numpy as np
import xml.etree.ElementTree as ET
from datetime import datetime, timedelta


def get_text(elem, tag):
    node = elem.find(".//" + tag)
    if node is None:
        raise ValueError(f"Tag {tag} not found")
    return node.text


def set_text(elem, tag, value):
    node = elem.find(".//" + tag)
    if node is None:
        raise ValueError(f"Tag {tag} not found in template")
    node.text = value


def parse_time(time_str):
    """Parse ISO time string to seconds since epoch."""
    # Format: 2023-04-03T21:40:36.338533Z
    dt = datetime.strptime(time_str.rstrip('Z'), "%Y-%m-%dT%H:%M:%S.%f")
    return dt.timestamp()


def format_time(t_sec, ref_time_str):
    """Format seconds since epoch back to ISO time string."""
    dt = datetime.fromtimestamp(t_sec)
    return dt.strftime("%Y-%m-%dT%H:%M:%S.%f") + "Z"


def lagrange_interp(x_old, y_old, x_new, order=7):
    """Lagrange interpolation matching ASP's order-7 (8-sample) scheme.
    For each x_new point, find the nearest 8 samples and interpolate."""
    n_old = len(x_old)
    radius = order // 2 + 1  # 4 for order 7
    y_new = np.zeros((len(x_new), y_old.shape[1]))

    for i, x in enumerate(x_new):
        # Find the nearest sample
        idx = np.searchsorted(x_old, x)
        # Center the window
        start = max(0, idx - radius)
        end = min(n_old, start + 2 * radius)
        start = max(0, end - 2 * radius)

        xs = x_old[start:end]
        ys = y_old[start:end]

        # Lagrange basis polynomials
        result = np.zeros(y_old.shape[1])
        for j in range(len(xs)):
            basis = 1.0
            for k in range(len(xs)):
                if k != j:
                    basis *= (x - xs[k]) / (xs[j] - xs[k])
            result += basis * ys[j]

        y_new[i] = result

    return y_new


def uniformize_ephemeris(neo_point_list, spot6_point_list):
    """Resample ephemeris onto uniform time grid."""
    neo_points = neo_point_list.findall("Point")
    n = len(neo_points)

    # Extract times and values
    times = []
    positions = []
    velocities = []
    for pt in neo_points:
        t = parse_time(pt.find("TIME").text)
        times.append(t)
        pos = [float(x) for x in pt.find("LOCATION_XYZ").text.split()]
        positions.append(pos)
        vel = [float(x) for x in pt.find("VELOCITY_XYZ").text.split()]
        velocities.append(vel)

    times = np.array(times)
    positions = np.array(positions)
    velocities = np.array(velocities)

    # Create uniform time grid
    t_start = times[0]
    t_end = times[-1]
    t_uniform = np.linspace(t_start, t_end, n)

    # Interpolate
    pos_uniform = lagrange_interp(times, positions, t_uniform)
    vel_uniform = lagrange_interp(times, velocities, t_uniform)

    # Report max interpolation shift
    max_dt = np.max(np.abs(t_uniform - times))
    print(f"  Ephemeris: max time shift = {max_dt*1e6:.2f} microseconds")

    # Clear old points and write new ones
    for old_pt in spot6_point_list.findall("Point"):
        spot6_point_list.remove(old_pt)

    ref_str = neo_points[0].find("TIME").text
    for i in range(n):
        pt = ET.SubElement(spot6_point_list, "Point")
        loc = ET.SubElement(pt, "LOCATION_XYZ")
        loc.text = f"{pos_uniform[i,0]:.15g} {pos_uniform[i,1]:.15g} {pos_uniform[i,2]:.15g}"
        vel = ET.SubElement(pt, "VELOCITY_XYZ")
        vel.text = f"{vel_uniform[i,0]:.15g} {vel_uniform[i,1]:.15g} {vel_uniform[i,2]:.15g}"
        t_elem = ET.SubElement(pt, "TIME")
        t_elem.text = format_time(t_uniform[i], ref_str)

    return n


def uniformize_quaternions(neo_quat_list, spot6_quat_list):
    """Resample quaternions onto uniform time grid."""
    neo_quats = neo_quat_list.findall("Quaternion")
    n = len(neo_quats)

    times = []
    qvals = []
    for q in neo_quats:
        t = parse_time(q.find("TIME").text)
        times.append(t)
        q0 = float(q.find("Q0").text)
        q1 = float(q.find("Q1").text)
        q2 = float(q.find("Q2").text)
        q3 = float(q.find("Q3").text)
        qvals.append([q0, q1, q2, q3])

    times = np.array(times)
    qvals = np.array(qvals)

    t_start = times[0]
    t_end = times[-1]
    t_uniform = np.linspace(t_start, t_end, n)

    q_uniform = lagrange_interp(times, qvals, t_uniform)

    # Normalize quaternions
    for i in range(n):
        norm = np.sqrt(np.sum(q_uniform[i] ** 2))
        q_uniform[i] /= norm

    max_dt = np.max(np.abs(t_uniform - times))
    print(f"  Quaternions: max time shift = {max_dt*1e6:.2f} microseconds")

    # Clear and write
    for old_q in spot6_quat_list.findall("Quaternion"):
        spot6_quat_list.remove(old_q)

    ref_str = neo_quats[0].find("TIME").text
    for i in range(n):
        q = ET.SubElement(spot6_quat_list, "Quaternion")
        status = ET.SubElement(q, "Q_STATUS")
        status.text = "NOMINAL"
        for j, tag in enumerate(["Q0", "Q1", "Q2", "Q3"]):
            elem = ET.SubElement(q, tag)
            elem.text = f"{q_uniform[i,j]:.17g}"
        t_elem = ET.SubElement(q, "TIME")
        t_elem.text = format_time(t_uniform[i], ref_str)

    return n


def main():
    if len(sys.argv) != 4:
        print(__doc__)
        sys.exit(1)

    neo_file, spot6_template, output_file = sys.argv[1:4]

    neo_tree = ET.parse(neo_file)
    neo_root = neo_tree.getroot()
    spot6_tree = ET.parse(spot6_template)
    spot6_root = spot6_tree.getroot()

    # 1. Image size
    neo_nrows = get_text(neo_root, "NROWS")
    neo_ncols = get_text(neo_root, "NCOLS")
    set_text(spot6_root, "NROWS", neo_nrows)
    set_text(spot6_root, "NCOLS", neo_ncols)
    print(f"Image size: {neo_ncols} x {neo_nrows}")

    # 2. Time_Range
    neo_time_range = neo_root.find(".//Time_Range")
    spot6_time_range = spot6_root.find(".//Time_Range")
    set_text(spot6_time_range, "START", get_text(neo_time_range, "START"))
    set_text(spot6_time_range, "END", get_text(neo_time_range, "END"))
    spot6_middle = spot6_time_range.find("MIDDLE")
    neo_middle = neo_time_range.find("MIDDLE")
    if spot6_middle is not None:
        if neo_middle is not None:
            spot6_middle.text = neo_middle.text
        else:
            spot6_time_range.remove(spot6_middle)

    # 3. LINE_PERIOD
    neo_lp = get_text(neo_root, "LINE_PERIOD")
    set_text(spot6_root, "LINE_PERIOD", neo_lp)
    print(f"LINE_PERIOD: {neo_lp} microseconds")

    # 4. Ephemeris - uniformized
    print("Uniformizing ephemeris:")
    neo_eph = neo_root.find(".//Ephemeris/Point_List")
    spot6_eph = spot6_root.find(".//Ephemeris/Point_List")
    n_eph = uniformize_ephemeris(neo_eph, spot6_eph)
    print(f"  Points: {n_eph}")

    # 5. Quaternions - uniformized
    print("Uniformizing quaternions:")
    neo_qlist = neo_root.find(".//Attitudes/Quaternion_List")
    spot6_qlist = spot6_root.find(".//Attitudes/Quaternion_List")
    n_quat = uniformize_quaternions(neo_qlist, spot6_qlist)
    print(f"  Quaternions: {n_quat}")

    # 6. Dataset_Extent
    neo_extent = neo_root.find(".//Dataset_Extent")
    spot6_extent = spot6_root.find(".//Dataset_Extent")
    for old_v in spot6_extent.findall("Vertex"):
        spot6_extent.remove(old_v)
    old_center = spot6_extent.find("Center")
    if old_center is not None:
        spot6_extent.remove(old_center)
    for v in neo_extent.findall("Vertex"):
        spot6_extent.append(v)
    neo_center = neo_extent.find("Center")
    if neo_center is not None:
        spot6_extent.append(neo_center)

    # 7. Look angles
    neo_look = neo_root.find(".//Band_Calibration/Polynomial_Look_Angles")
    spot6_look = spot6_root.find(".//Band_Calibration/Polynomial_Look_Angles")
    for tag in ["XLOS_0", "XLOS_1", "YLOS_0", "YLOS_1"]:
        neo_val = get_text(neo_look, tag)
        set_text(spot6_look, tag, neo_val)

    # 8. Swath_Range LAST_COL
    spot6_last_col = spot6_root.find(".//Swath_Range/LAST_COL")
    if spot6_last_col is not None:
        spot6_last_col.text = neo_ncols

    # Write
    spot6_tree.write(output_file, encoding="UTF-8", xml_declaration=True)
    print(f"Wrote: {output_file}")


if __name__ == "__main__":
    main()
