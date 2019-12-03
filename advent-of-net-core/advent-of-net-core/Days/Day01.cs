using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace AdventOfCode2019 {
  class Day01 {

    internal async Task solve() {
      var lines = await File.ReadAllLinesAsync("../input/day01.txt");
      var (part1, part2) = runPuzzle(lines.Select(int.Parse).ToList());
      await Console.Out.WriteLineAsync($"Part 1:\n{part1}");
      await Console.Out.WriteLineAsync($"Part 2:\n{part2}");
    }

    (int part1, int part2) runPuzzle(IEnumerable<int> modules) {
      return modules.Select(getRequiredFuel)
        .Aggregate(
          (0, 0),
          ((int sum1, int sum2) s, (int fuel1, int fuel2) f) =>
            (s.sum1 + f.fuel1, s.sum2 + f.fuel2)
        );
    }

    (int fuelSimple, int fuelTyrannical) getRequiredFuel(int moduleWeight) {
      var fuelSimple = fuelRequired(moduleWeight);
      return (fuelSimple, fuelRecursive(fuelSimple, fuelSimple));
    }

    int fuelRequired(int weight) => weight / 3 - 2;

    int fuelRecursive(int weight, int total) =>
      fuelRequired(weight) switch {
        var fuel when fuel <= 0 => total,
        var fuel                => fuelRecursive(fuel, total + fuel)
      };
  }
}