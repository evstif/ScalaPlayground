val max = 30

for ( i <- -1 to max )
  yield (i, ((i + 1) % max))
