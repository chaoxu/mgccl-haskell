integerPartitions n = part n n
  where part 0 _ = [[]]
        part n k = concat [map (i:) (part (n-i) i) | i<-[1..min k n]]
