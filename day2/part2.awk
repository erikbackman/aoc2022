/A X/ { s+=3 }
/B X/ { s+=1 }
/C X/ { s+=2 }

/A Y/ { s+=4 }
/B Y/ { s+=5 }
/C Y/ { s+=6 }

/A Z/ { s+=8 }
/B Z/ { s+=9 }
/C Z/ { s+=7 }

END { print s }
