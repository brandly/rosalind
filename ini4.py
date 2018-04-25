def is_odd(x):
    return x % 2 == 1

def answer(a, b):
    return sum(filter(is_odd, range(a, b + 1)))

print(answer(100, 200))

