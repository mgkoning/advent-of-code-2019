using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace AdventOfCode2019 {

  class Program {
    static Func<Task>[] solvers = new Func<Task>[] {
      async () => await new Day01().solve(),
      async () => await new Day02().solve()
    };

    static async Task Main(string[] args) {
      var dayToRun = 0 < args.Length ? int.Parse(args[0]) : DateTime.Now.AddHours(-6).Day;
      var solve = solvers.TryGetValue(dayToRun - 1);
      if (solve == null) {
        Console.Error.WriteLine($"Day {dayToRun} not supported.");
        Environment.Exit(1);
      }
      Console.WriteLine($"Running day {dayToRun}.");
      await solve();
    }

  }

}
