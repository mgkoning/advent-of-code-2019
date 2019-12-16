using System;
using System.Threading.Tasks;
using System.IO;
using System.Linq;
using System.Collections.Generic;

namespace AdventOfCode2019 {
  class Day16 {
    internal async Task solve() {
      var input = (await File.ReadAllTextAsync("../input/day16.txt"));
      var offset = int.Parse(input.Substring(0, 7));
      var digits = input.Select(s => int.Parse(s.ToString())).ToList();
      var repeatedSignal = new List<int>(10000 * digits.Count);
      for (var i = 0; i < 10000; i++) {
        repeatedSignal.AddRange(digits);
      }
      var forSolution = repeatedSignal.Skip(offset).ToList();
      var transformed = forSolution.ToList();
      for (var iteration = 0; iteration < 100; iteration++) {
        var sum = 0;
        for (var index = forSolution.Count - 1; index >= 0; index--) {
          sum = (sum + transformed[index]) % 10;
          transformed[index] = sum;
        }
      }
      await Console.Out.WriteLineAsync($"Part 2: {string.Join("", transformed.Take(8).Select(s => s.ToString()))}");
    }
  }
}