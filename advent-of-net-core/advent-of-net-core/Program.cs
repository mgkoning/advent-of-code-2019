using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace AdventOfCode2019 {

  class Program {
    static Dictionary<int, Func<Task>> solvers = new Dictionary<int, Func<Task>> {
      [1] = async () => await new Day01().solve()
    };

    static async Task Main(string[] args) {
      var dayToRun = 0 < args.Length ? int.Parse(args[0]) : DateTime.Now.AddHours(-6).Day;
      if (!solvers.TryGetValue(dayToRun, out var solve)) {
        Console.Error.WriteLine($"Day {dayToRun} not supported.");
        Environment.Exit(1);
      }
      Console.WriteLine($"Running day {dayToRun}.");
      await solve();
    }

  }
}
