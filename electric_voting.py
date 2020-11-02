#!/usr/bin/env python

# Copyright 2020 Google LLC
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""Experiments with implementation of the voting system."""

import logging
import numpy
# from typing import List, Tuple

def solve(votes: numpy.ndarray) -> numpy.ndarray:
    trans = numpy.transpose(votes)
    sums = votes + trans
    print("Sums: ", sums)
    sums_vector = numpy.sum(sums, axis=1)
    diffs = numpy.sum(votes - trans, axis=1)
    print("Diffs: ", diffs)
    coefs = sums - numpy.diag(sums_vector)
    print("Coefs: ", coefs)
    dim = coefs.shape[0]
    coefs = numpy.delete(coefs, 0, axis=0)
    coefs = numpy.delete(coefs, dim - 1, axis=1)
    diffs = numpy.delete(diffs, 0)
    print("Diffs: ", diffs)
    print("Solving: ", coefs, diffs)
    return numpy.linalg.solve(coefs, diffs)

def main():
    #votes = numpy.array([[0, 1], [0, 0]])
    # https://en.wikipedia.org/wiki/Condorcet_method#Example:_Voting_on_the_location_of_Tennessee's_capital
    votes = [[ 0, 42, 42, 42],
             [58,  0, 68, 68],
             [58, 32,  0, 83],
             [58, 32, 17,  0]]
    print(solve(votes))

if __name__== "__main__":
    main()
