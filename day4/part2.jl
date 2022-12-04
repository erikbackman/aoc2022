input = readlines("input.txt")
re = r"(?<a>\d+)-(?<b>\d+),(?<c>\d+)-(?<d>\d+)"

mapreduce(line ->
    let
          a,b,c,d = map(s -> parse(Int, s), match(re, line));
          i1 = [a:b;];
          i2 = [c:d;];
          return !iszero(intersect(i1, i2))
    end
    ,+,input)
