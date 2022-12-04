match($0, /([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)/, m) {
    a=m[1]; b=m[2]; c=m[3]; d=m[4];
    intersect=0
    for (i = a; i <= b; i++) {
	if (i >= a && i <= b) {
	    intersect++
	    break
	}
    }
    for (i = m[1]; i <= m[2]; i++) {
	if (i >= c && i <= d) {
	    intersect++
	    break;
	}
    }
    s += (intersect == 2)
} END { print s }
