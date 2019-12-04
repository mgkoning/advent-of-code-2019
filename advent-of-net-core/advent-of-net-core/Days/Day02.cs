using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace AdventOfCode2019 {

  class Day02 {

    internal async Task solve() {
      var program = (await File.ReadAllTextAsync("../input/day02.txt")).Split(',').Select(int.Parse).ToList();
      var state = runProgram(program, 12, 2);
      await Console.Out.WriteLineAsync($"Part 1:\n{state[0]}");
      var (verb, noun) = findInput(program, 19690720);
      await Console.Out.WriteLineAsync($"Part 2:\n{verb*100 + noun}");
      
    }

    private (int verb, int noun) findInput(IList<int> program, int desiredAt0) {
      for(var verb = 0; verb < 100; verb++) {
        for(var noun = 0; noun < 100; noun++) {
          var state = runProgram(program, verb, noun);
          if (state[0] == desiredAt0) { return (verb, noun); }
        }
      }
      throw new IndexOutOfRangeException();
    }

    internal IList<int> runProgram(IList<int> program, int pos1, int pos2) {
      var pointer = 0;
      program = program.ToList(); // make a copy
      (program[1], program[2]) = (pos1, pos2);
      while (program.hasIndex(pointer)) {
        var done = program[pointer] switch {
          1 => binOp((a,b) => a + b),
          2 => binOp((a,b) => a * b),
          99 => true,
          _ => throw new Exception($"Unknown opcode {program[pointer]}")
        };
        if (done)  { return program; }
      }
      throw new IndexOutOfRangeException();

      bool binOp(Func<int, int, int> op) {
        program[program[pointer + 3]] = op(program[program[pointer+1]], program[program[pointer+2]]);
        pointer += 4;
        return false;
      }
    }

  }
}