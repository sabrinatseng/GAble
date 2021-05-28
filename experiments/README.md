## Benchmarks used in the paper: 
- MultiFilter
- InsertionSort2
- QuickSort

The .hs files show an example of what the synthesized program might look like. .out files go along with the PNGs, shows all of the values from each trial near the end of the file.

## Other experiments:
- AbsoluteSum

Another benchmark we wanted to add, but didn't finish in time to put it in the paper. It took a very long time to run (longer than the others with similar search spaces), have not yet investigated why

- disallowing duplicates

insertion_sort_20_20_d was an experiment with disallowing duplicates in the chromosome. With the current implementation, duplicate program pieces would always be incorrect, but I am not sure if this would be true in general in this model. This uses a different insertion sort (in insertion_sort.hs) with a larger search space.

- scatter plots

The quicksort scatter plots show the fitness values over time during GP for the different fitness functions.