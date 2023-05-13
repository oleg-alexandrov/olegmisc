#!/usr/bin/env python

"""
Given two files of numbers in the format <digits>.<digits> on each line, with some other things on the line as well that will be ignored, for each value in the first file find the closest value in the second file. 
"""

import argparse, os, re, sys 

def read_file(filename):
    """Return sorted values."""

    vals = []
    files = {}
    with open(filename, 'r') as f:
        lines = f.readlines()

    for line in lines:
        m = re.match("^.*?(\d+\.\d+).*?\n", line)
        if m:
            timestamp = float(m.group(1))
            vals.append(timestamp)
        files[timestamp] = line.rstrip()
            
    return (sorted(vals), files)

def find_closest_messages_within_tol(args, topic_list, timestamp_map):
    """
    Given a list of timestamps and a tolerance, find, for each input
    topic, the closest message in the bag to every timestamp in the
    list, within given tolerance.
    """
    print("Doing a first pass through the bag to find the closest messages to "
          "desired ones, with tolerance.")

    # The timestamps close to which need to find data in the bag, Must
    # be sorted.
    req_timestamps = sorted(list(timestamp_map.keys()))
    min_list_time = req_timestamps[0]
    max_list_time = req_timestamps[-1]

    # This will travel forward in time, and we assume the messages travel
    # forward in time too.
    # Must depend on topic!
    req_index = {}
    closest_stamps = {}
    first_good = {}
    for topic in topic_list:
        req_index[topic] = 0
        first_good[topic] = -1
        # Will keep the result here, starts as an array with values smaller
        # than any timestamp
        closest_stamps[topic] = [-1000.0] * len(req_timestamps)
    
    with rosbag.Bag(args.bag, "r") as bag:
        
        # Check image message type
        for topic, msg, t in bag.read_messages(topic_list):
            
            # Read the header timestamp
            try:
                # Note that we search the bag exhaustively. We do not assume
                # timestamps are in increasing order of time. Sometimes
                # that assumption can be violated.
                stamp = msg.header.stamp.to_sec()
                if stamp < min_list_time - args.timestamp_tol:
                    continue # too early
                if stamp > max_list_time + args.timestamp_tol:
                    break # past the desired times

                # See if this timestamp is closer to any of the req_timestamps
                # than the existing candidates. Note that we stop as soon as
                # we can to keep the overall complexity linear and not quadratic.
                first_good[topic] = -1
                for it in range(req_index[topic], len(req_timestamps)):
                    if req_timestamps[it] > stamp + args.timestamp_tol:
                        break # no point in continuing

                    curr_diff = abs(stamp - req_timestamps[it])
                    prev_diff = abs(closest_stamps[topic][it] - req_timestamps[it])
                    if curr_diff < args.timestamp_tol and curr_diff < prev_diff:
                        closest_stamps[topic][it] = stamp

                        if first_good[topic] < 0:
                            first_good[topic] = 1
                            # Found the first good fit index. There won't be good
                            # fits going forward for indices to the left of this. 
                            req_index[topic] = it
            except:
                continue

    # Put in a set and print some stats
    closest_stamps_set = {}
    exact_to_approx = {}
    for topic in topic_list:
        print("Topic is " + str(topic))
        closest_stamps_set[topic] = set()
        exact_to_approx[topic] = {}
        for it in range(len(closest_stamps[topic])):
            diff = abs(closest_stamps[topic][it] - req_timestamps[it])
            msg = ""
            if diff > args.timestamp_tol:
                diff = "-1"
                msg = " (failed)"
            else:
                closest_stamps_set[topic].add(closest_stamps[topic][it])
                exact_to_approx[topic][closest_stamps[topic][it]] = req_timestamps[it] 
            print("For timestamp " + str(req_timestamps[it]) + \
                  ", closest found message is within " + str(diff) + " seconds" + msg)
        
    return (closest_stamps_set, exact_to_approx)

if len(sys.argv) < 3:
    print("Must specify two input files.")
    
file1 = sys.argv[1]
file2 = sys.argv[2]

(src, src_files) = read_file(file1)
(ref, ref_files) = read_file(file2)

# The timestamp format is consistent with what rig_calibrator uses
fmt_str = "{:10.7f}"

# Assuming src and ref are sorted in increasin gorder
start_pos = 0 # position in second file from where we start searching, this will go forward
for it in range(len(src)):
    best_pos = 0
    best_dist = abs(ref[0]) + abs(ref[-1]) + abs(src[0]) + abs(src[-1]) # big value

    #print("\n---try it, ", it)
    for pos in range(start_pos, len(ref)):

        #print("--pos, ", pos)
        #print("ref - src", ref[pos] - src[it])
        
        if ref[pos] < src[it]:
            start_pos = pos # for next time, there is no point in starting more on the left
            #print("--next time start at ", start_pos)
            
        dist = abs(src[it] - ref[pos])
        if dist < best_dist:
            best_pos = pos
            best_dist = dist
            #print("--best pos and dist ", best_pos, dist)
            
        if ref[pos] > src[it]:
            # distances will get only bigger
            #print("giving up")
            break

    print("closest to " + src_files[src[it]] + " is " + ref_files[ref[best_pos]] + " with dist " + str(best_dist))
    
            
