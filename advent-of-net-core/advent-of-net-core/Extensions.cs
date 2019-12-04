using System;
using System.Collections;
using System.Collections.Generic;

namespace AdventOfCode2019 {

  public static class Extensions {

    public static T? TryGetValue<T>(this IList<T> @this, int index) where T: class {
      return hasIndex(@this, index) ? @this[index] : null;
    }
    
    public static bool hasIndex<T>(this IList<T> @this, int index) {
      return 0 <= index && index < @this.Count;
    }

  }
}