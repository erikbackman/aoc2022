using Base.Iterators, Printf
lines = readlines("input.txt")
sacks = map(l -> let ix = Int(length(l)/2); collect(partition(l, ix)) end, lines)
prio(c) = c <= 'Z' ? Int(c) - Int('A') + 27 : Int(c) - Int('a') + 1
part1 = mapreduce(prio, +, map(v -> join(intersect(v[1], v[2]))[1], sacks))
groups = partition(lines, 3)
part2 = mapreduce(g -> prio(join(intersect(g[1], g[2], g[3]))[1]), +, groups)
@printf("p1: %d p2: %d\n", part1, part2)
