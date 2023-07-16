namespace KaiEkkrin.FsData.Core;

internal class Common
{
    internal static int DivCeil(int a, int b)
    {
        var (quotient, remaineder) = Math.DivRem(a, b);
        return remaineder == 0 ? quotient : quotient + 1;
    }
}
