n <- 100 #subject
u <- 0.01 # m^3 brain volume loss mean
s <- 0.04 # m^3 brain volume loss std. dev.
p <- 0.05 # sign level

x <- power.t.test(power = 0.9, delta = u, sd = s, sig.level = p, type = "one.sample", alt = "one.sided")$n
ceiling(x / 10) * 10