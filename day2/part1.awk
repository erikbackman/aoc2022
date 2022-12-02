/A X/ { s+=4 }
/A Y/ { s+=8 }
/A Z/ { s+=3 }
/B X/ { s+=1 }
/B Y/ { s+=5 }
/B Z/ { s+=9 }
/C X/ { s+=7 }
/C Z/ { s+=6 }
/C Y/ { s+=2 }

END { printf "%d\n", s }
