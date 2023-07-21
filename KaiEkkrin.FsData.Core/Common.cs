using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace KaiEkkrin.FsData.Core;

/// <summary>
/// Trivial stuff used by the rest
/// </summary>
internal static class Common
{
    internal static int DivCeil(int a, int b)
    {
        var (div, rem) = Math.DivRem(a, b);
        return rem == 0 ? div : div + 1;
    }
}
