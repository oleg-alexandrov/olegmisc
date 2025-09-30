#!/usr/bin/python

import matplotlib.pyplot as plt
import sys

def plot_values_from_file(filepath):
    """
    Reads numerical values from a file (one per line) and plots them.

    The x-axis represents the index of the value in the file, and the
    y-axis represents the value itself.
    """
    values = []
    print(f"Attempting to open and read file: {filepath}")

    try:
        with open(filepath, 'r') as f:
            for line in f:
                # Strip whitespace from the beginning/end of the line
                clean_line = line.strip()
                if clean_line:  # Ensure the line is not empty
                    try:
                        # Convert the line to a number (float) and add to our list
                        values.append(float(clean_line))
                    except ValueError:
                        print(f"Warning: Skipping non-numeric line: '{clean_line}'")

        # --- Plotting the data ---
        if not values:
            print("No valid data found to plot.")
            return

        print(f"Successfully read {len(values)} data points. Now plotting...")

        # Create a plot figure with a specific size for better viewing
        plt.figure(figsize=(10, 6))

        # Plot the values. Matplotlib uses the list index as the x-value by default.
        # Removed markers for a continuous line plot.
        plt.plot(values, linestyle='-', color='b')

        # Add labels to the axes and a title to the plot for clarity
        plt.title('Data from Input File')
        plt.xlabel('Index')
        plt.ylabel('Value')

        # Add a grid to make the plot easier to read
        plt.grid(True)

        # Display the plot
        plt.show()

    except FileNotFoundError:
        print(f"Error: The file '{filepath}' was not found. Please check the file path.")
    except Exception as e:
        print(f"An unexpected error occurred: {e}")

if __name__ == '__main__':
    # Check if a filename is provided as a command-line argument
    if len(sys.argv) < 2:
        print("Usage: python plot_from_file.py <path_to_file>")
        sys.exit(1)

    # The input file is taken from the first command-line argument.
    input_file = sys.argv[1]
    plot_values_from_file(input_file)

