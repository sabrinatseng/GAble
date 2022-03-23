## Benchmarks used in the paper: 

For all problems below, the `.hs` files show an example of what the synthesized program might look like. The `.out` files go along with the PNGs, showing all of the values from each trial for number of generations taken to find a solution.

### MultiFilter

Note: this is called `MultiFilter3` in the main [`Gable.hs`](../Gable.hs) file.

Problem: Given a list of integers, filter the list and return 3 new lists containing just the even integers, just the odd integers, and just the integers greater than 2. 

Program pieces:
- A main piece that takes a list and filters into 3 lists based on 3 predefined condition functions.
- 3 possible conditions (`isEven`, `isOdd`, `isGTTwo`) for each condition function.

Results:
- [Example code](multi_filter.hs)
- [Results (txt)](multi_filter_20_20_60.out)
- [Graph](multi_filter_20_20_60.png)

### InsertionSort2

Based on the insertion sort example in [this LiquidHaskell blog post](http://goto.ucsd.edu/~gridaphobe/liquid/haskell/blog/blog/2013/07/29/putting-things-in-order.lhs/).

Problem: Given a list of integers, return a sorted list (in increasing order) using insertion sort.

Program pieces:
- A high level `insertSort` piece which inserts the first value into the recursively-sorted rest of the list.
- An `insert` piece which inserts a value into a sorted list based on a predefined boolean operator and 2 predefined condition functions. This piece along with `insertSort` provide the control flow.
- 3 possible conditions (<=, >=, ==) for each condition function
- 2 possible boolean operators (and, or)

Note: the correct condition only requires <= so it doesn't require two separate conditions with a boolean operator. We added this complication to artificially increase the search space size without increasing the problem complexity too much.

Results:
- [Example code](insertion_sort_2.hs)
- [Results (txt)](insertion_sort_2_20_20.out)
- [Graph](insertion_sort_2_20_20.png)

### QuickSort

Based on the quicksort example in [the same blog post from above](http://goto.ucsd.edu/~gridaphobe/liquid/haskell/blog/blog/2013/07/29/putting-things-in-order.lhs/).

Problem: Given a list of integers, return a sorted list (in increasing order) using quicksort.

Program pieces:
- A high level `quickSort` piece which chooses the first value `x` as the pivot, filters the list into `lts` and `gts` based on 2 predefined condition functions, and constructs the output list using the `pivApp` piece. 
- A `pivApp` piece which concatenates `lts`, `x`, and `gts`.
- 3 possible conditions (<=, >=, ==) for each condition function

Results:
- [Example code](quicksort.hs)
- [Results (txt)](quicksort_20_20.out)
- [Graph](quicksort_20_20.png)

## Other experiments:
- AbsoluteSum

Another benchmark we wanted to add, but didn't finish in time to put it in the paper. It took a very long time to run (longer than the others with similar search spaces), have not yet investigated why

- disallowing duplicates

`insertion_sort_20_20_d` was an experiment with disallowing duplicates in the chromosome. With the current implementation, duplicate program pieces would always be incorrect, but I am not sure if this would be true in general in this model. This uses a different insertion sort (in insertion_sort.hs) with a larger search space.

- scatter plots

The `quicksort` scatter plots show the fitness values over time during GP for the different fitness functions.