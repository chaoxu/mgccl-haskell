integerPartitions n = part n n
  where part 0 _ = [[]]
        part n k = [(i:is) | i<-[1..min k n], is <- part (n-i) i]
