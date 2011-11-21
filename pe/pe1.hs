result = sum [n| n<-[1..999],n `mod` 3 == 0 || n `mod` 5==0]
