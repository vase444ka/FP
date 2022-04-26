main :: IO ()
main = print (fac 5)

fac n = if n == 0 then 1 else n * fac (n-1)

doubleMe x = x*2