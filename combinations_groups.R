# How many ways to split 4 people into 2 groups, e.g.
# 0   0   0 | 0
choose(4 - 1, 2 - 1)
# list the possible divider positions
t(combn(4 - 1, 2 - 1))

# How many ways to split 4 people into 3 groups, e.g.
# 0 | 0   0 | 0
choose(4 - 1, 3 - 1)
# list the possible divider positions
t(combn(4 - 1, 3 - 1))
# 0 | 0 | 0   0 
# 0 | 0   0 | 0
# 0   0 | 0 | 0 

