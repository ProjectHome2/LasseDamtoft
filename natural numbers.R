n = 0
count = 0 
repeat{
  n = n + 1
  count = count + n
  if (count + 1/12 < 0.01) {
    break
  }
  cat("count", count, "\n")
}
