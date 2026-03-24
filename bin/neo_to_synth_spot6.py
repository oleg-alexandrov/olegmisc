#!/usr/bin/env python3

"""Convert a Pleiades Neo DIM XML into a synthetic SPOT6 DIM XML.

Keeps the SPOT6 XML skeleton (S6_SENSOR profile, all SPOT6-specific
metadata) but replaces the camera-relevant numeric sections with
values from the Neo XML:
  - Time_Range (START, END, MIDDLE)
  - LINE_PERIOD
  - NROWS, NCOLS
  - Ephemeris Point_List
  - Quaternion_List
  - Dataset_Extent vertices and center
  - Polynomial_Look_Angles (XLOS_0, XLOS_1, YLOS_0, YLOS_1)
  - Swath_Range LAST_COL

Usage:
  python neo_to_synth_spot6.py <neo_dim.xml> <spot6_template.xml> <output.xml>

The SPOT6 template is the real SPOT6 DIM XML used as the skeleton.
"""

import sys
import xml.etree.ElementTree as ET


def get_text(elem, tag):
    """Get text of a descendant tag."""
    node = elem.find(".//" + tag)
    if node is None:
        raise ValueError(f"Tag {tag} not found")
    return node.text


def set_text(elem, tag, value):
    """Set text of a descendant tag."""
    node = elem.find(".//" + tag)
    if node is None:
        raise ValueError(f"Tag {tag} not found in template")
    node.text = value


def find_section(root, path):
    """Find a section by path, trying with and without namespace."""
    node = root.find(path)
    if node is None:
        node = root.find(".//" + path.split("/")[-1])
    return node


def main():
    if len(sys.argv) != 4:
        print(__doc__)
        sys.exit(1)

    neo_file, spot6_template, output_file = sys.argv[1:4]

    # Parse both XMLs
    neo_tree = ET.parse(neo_file)
    neo_root = neo_tree.getroot()

    spot6_tree = ET.parse(spot6_template)
    spot6_root = spot6_tree.getroot()

    # 1. Replace NROWS and NCOLS
    neo_nrows = get_text(neo_root, "NROWS")
    neo_ncols = get_text(neo_root, "NCOLS")
    set_text(spot6_root, "NROWS", neo_nrows)
    set_text(spot6_root, "NCOLS", neo_ncols)
    print(f"Image size: {neo_ncols} x {neo_nrows}")

    # 2. Replace Time_Range (START, END, MIDDLE if present)
    neo_time_range = neo_root.find(".//Time_Range")
    spot6_time_range = spot6_root.find(".//Time_Range")
    set_text(spot6_time_range, "START", get_text(neo_time_range, "START"))
    set_text(spot6_time_range, "END", get_text(neo_time_range, "END"))
    # MIDDLE may not exist in Neo; compute from START/END if needed
    spot6_middle = spot6_time_range.find("MIDDLE")
    neo_middle = neo_time_range.find("MIDDLE")
    if spot6_middle is not None:
        if neo_middle is not None:
            spot6_middle.text = neo_middle.text
        else:
            # Remove MIDDLE from template since Neo doesn't have it
            spot6_time_range.remove(spot6_middle)

    # 3. Replace LINE_PERIOD
    neo_lp = get_text(neo_root, "LINE_PERIOD")
    set_text(spot6_root, "LINE_PERIOD", neo_lp)
    print(f"LINE_PERIOD: {neo_lp} microseconds")

    # 4. Replace Ephemeris Point_List
    neo_ephemeris = neo_root.find(".//Ephemeris")
    spot6_ephemeris = spot6_root.find(".//Ephemeris")
    neo_point_list = neo_ephemeris.find("Point_List")
    spot6_point_list = spot6_ephemeris.find("Point_List")

    neo_points = neo_point_list.findall("Point")
    print(f"Ephemeris points: {len(neo_points)}")

    # Clear old points and copy new ones
    for old_pt in spot6_point_list.findall("Point"):
        spot6_point_list.remove(old_pt)
    for pt in neo_points:
        spot6_point_list.append(pt)

    # 5. Replace Quaternion_List
    neo_attitudes = neo_root.find(".//Attitudes")
    spot6_attitudes = spot6_root.find(".//Attitudes")
    neo_quat_list = neo_attitudes.find("Quaternion_List")
    spot6_quat_list = spot6_attitudes.find("Quaternion_List")

    neo_quats = neo_quat_list.findall("Quaternion")
    print(f"Quaternions: {len(neo_quats)}")

    # Clear old quaternions and copy new ones
    for old_q in spot6_quat_list.findall("Quaternion"):
        spot6_quat_list.remove(old_q)
    for q in neo_quats:
        spot6_quat_list.append(q)

    # 6. Replace Dataset_Extent vertices and center
    neo_extent = neo_root.find(".//Dataset_Extent")
    spot6_extent = spot6_root.find(".//Dataset_Extent")

    # Remove old vertices and center
    for old_v in spot6_extent.findall("Vertex"):
        spot6_extent.remove(old_v)
    old_center = spot6_extent.find("Center")
    if old_center is not None:
        spot6_extent.remove(old_center)

    # Copy new vertices and center
    for v in neo_extent.findall("Vertex"):
        spot6_extent.append(v)
    neo_center = neo_extent.find("Center")
    if neo_center is not None:
        spot6_extent.append(neo_center)

    # 7. Replace Polynomial_Look_Angles
    neo_look = neo_root.find(".//Band_Calibration/Polynomial_Look_Angles")
    spot6_look = spot6_root.find(".//Band_Calibration/Polynomial_Look_Angles")
    for tag in ["XLOS_0", "XLOS_1", "YLOS_0", "YLOS_1"]:
        neo_val = get_text(neo_look, tag)
        set_text(spot6_look, tag, neo_val)
        print(f"  {tag}: {neo_val}")

    # 8. Replace Swath_Range LAST_COL to match new NCOLS
    spot6_last_col = spot6_root.find(".//Swath_Range/LAST_COL")
    if spot6_last_col is not None:
        spot6_last_col.text = neo_ncols
        print(f"LAST_COL: {neo_ncols}")

    # Write output
    spot6_tree.write(output_file, encoding="UTF-8", xml_declaration=True)
    print(f"Wrote: {output_file}")


if __name__ == "__main__":
    main()
