---
layout:   post
title:    Sorted Containers in Ruby inspired by Python
summary:  A new Sorted Containers gem for Ruby
date:     2024-04-26 23:47:06
---

## Quick Links

- sorted_containers on [Github](https://github.com/GarrisonJ/sorted_containers)
- sorted_containers on [RubyGems](https://rubygems.org/gems/sorted_containers)
- sorted_containers [Documentation](https://www.rubydoc.info/gems/sorted_containers)

## Introduction

I converted Grant Jenks's Python library [Sorted Containers](http://www.grantjenks.com/docs/sortedcontainers/) to Ruby. If you are interested in the details of this data structure, I recommend reading his website. His documentation is much more detailed, and I used it as a reference for this implementation.

The library provides a fast sorted array, sorted set, and sorted hash implemented in pure Ruby with no dependencies.

SortedArray, SortedSet, and SortedHash are meant to be drop-in replacements for Array, Set, and Hash but with the extra property that the elements can be accessed in sorted order. 

I compare the performance of SortedContainers to [SortedSet](https://github.com/knu/sorted_set) a C extension red-black tree implementation. You can see the benchmarks below. The performance is comparable for add and delete, and much better for iteration, initialization, and lookup. 

Some methods from Array, Set, and Hash have not been implemented in SortedContainers. I want to complete these before version 1.0.0. Feel free to open an issue if you would like a method added or add a pull request if you would like to contribute.

Feedback is welcome. I hope you find this library useful.

## How it works

Modern computers are good at shifting arrays. For that reason, it's often faster to keep an array sorted than to use the usual tree-based data structures.

For example, if you have the array `[1,2,4,5]` and want to insert the element `3`, you can shift `4, 5` to the right and insert `3` in the correct position. This is a `O(n)` operation, but in practice it's fast.

You also save memory by not having to store pointers to children nodes, and you benifit from the cache locality of arrays. When you iterate over a sorted array, you are more likely to access elements that are close together in memory.

But we can do better if we have a lot of elements. We can break up the array so fewer elements have to be moved when a new element is inserted. For example, if you have the array `[[1,2,4],[5,6,7]]` and you want to insert the element `3`, you can insert `3` into the first array to get `[[1,2,3,4],[5,6,7]]` and only the element `4` has to be shifted.

This often outperforms the more common tree-based data structures like red-black trees with `O(log n)` insertions, deletions, and lookup. We sacrifice theoretical time complexity for practical performance.

The size of the subarrays is a trade-off. You can modify how big you want to subarrays by setting the `load_factor`. The default is set to `DEFAULT_LOAD_FACTOR = 1000`. The subarray is split when its size is `2*load_factor`. There is no perfect value. The ideal value will depend on your use case and may require some experimentation.

SortedSet and SortedHash are implemented using a SortedArray to keep track of the order, and then also use a standard Set and Hash for quick lookups. 

## Caveats

- Items must be comparable. If you try to insert an element that is not comparable, you will get an error.
- The order of the items must not change after they are inserted, or the container will be corrupted.

## Benchmarks

[SortedSet](https://github.com/knu/sorted_set) is a C extension red-black tree implementation. I used it as a benchmark to compare the performance of SortedContainers.

Every test was run 5 times and the average was taken.

You can see that SortedContainers has compariable performance for add and delete, and much better performance for iteration, initialization, and include.

I did my best to make the tests as fair as possible, but it's possible that I made a mistake. It's also difficult to predict real-world performance from these tests. If you have any suggestions for improvement, please let me know by opening an issue. The code for the benchmarks can be found in the github repository.

- MacBook Pro (16-inch, 2019)
- 2.6 GHz 6-Core Intel Core i7, 16 GB 2667 MHz DDR4
- Ruby 3.2.2
- SortedContainers 0.1.0
- SortedSet 1.0.3

### Results (Lower is better)

<figure>
    <label for="initalize-performance" class="margin-toggle">⊕</label><input type="checkbox" id="initalize-performance" class="margin-toggle"><span class="marginnote">Initialize a Sorted Set with <em>N</em> elements</span>
          <img src="https://github.com/GarrisonJ/sorted_containers/blob/main/benchmark/initialize_performance_comparison.png?raw=true" alt="Graph showing Initialization performance comparison">
</figure>

<figure>
    <label for="add-performance" class="margin-toggle">⊕</label><input type="checkbox" id="add-performance" class="margin-toggle"><span class="marginnote">Add <em>N</em> elements to a Sorted Set</span>
          <img src="https://github.com/GarrisonJ/sorted_containers/blob/main/benchmark/add_performance_comparison.png?raw=true" alt="Graph showing Add performance comparison">
</figure>

<figure>
    <label for="delete-performance" class="margin-toggle">⊕</label><input type="checkbox" id="delete-performance" class="margin-toggle"><span class="marginnote">Delete <em>N</em> elements from a Sorted Set</span>
          <img src="https://github.com/GarrisonJ/sorted_containers/blob/main/benchmark/delete_performance_comparison.png?raw=true" alt="Graph showing Delete performance comparison">
</figure>

<figure>
    <label for="iteration-performance" class="margin-toggle">⊕</label><input type="checkbox" id="iteration-performance" class="margin-toggle"><span class="marginnote">Iterate over a Sorted Set with <em>N</em> elements</span>
          <img src="https://github.com/GarrisonJ/sorted_containers/blob/main/benchmark/iteration_performance_comparison.png?raw=true" alt="Graph showing Iteration performance comparison">
</figure>

<figure>
    <label for="include-performance" class="margin-toggle">⊕</label><input type="checkbox" id="include-performance" class="margin-toggle"><span class="marginnote">Check if <em>N</em> elements are included in a Sorted Set</span>
          <img src="https://github.com/GarrisonJ/sorted_containers/blob/main/benchmark/include_performance_comparison.png?raw=true" alt="Graph showing Include performance comparison">
</figure>

## Conclusion

Feedback is welcome. Please open an issue on [Github](https://github.com/GarrisonJ/sorted_containers) if you have any suggestions or find any bugs.

If you like Python libraries converted to Ruby, you might also like my conversion of `heapq` to Ruby called [heapify](https://github.com/GarrisonJ/heapify)