lines=readlines("input2.txt")
A = [3 4 8 1 5 9 2 6 7]
X = zeros(Int32, 9)
foreach(v -> X[(Int(v[1]) - Int('A'))*3 + Int(v[3]) - Int('X') + 1]+=1, lines)
println((A*X)[1])
