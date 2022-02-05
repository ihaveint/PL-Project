def f(n = 0):
    a = n;
    b = n + 1;
    return a ** b;
;

def g():
    c = 1 < 7 and 13 > 17 or 1 == 1;
    print(c);
    return c;
;

l = [1, 3, 5, 7];

if g():
    h = f;
    d = h(3);
    print(d);
else:
    print(0);
;

x = 0;
if False:
    x = l[2];
else:
    x = l[3];
;
print(x);
print(0 * 3);
print(4 * 3);

def evil_function():
    print(123123);
    return 3;
;

lazy_arr = [1, evil_function(), 4, 5];
print(1010101010);
print(lazy_arr);

def unwanted_function():
    print(1111111);
    return 222222;
;

def good_func(a_function = None, decision = 0):
    if decision == 0:
        return 4;
    else:
        aa = a_function();
        return aa;
    ;
;

h = unwanted_function;
g = good_func(h, 0);
print(g);

h = unwanted_function;
g = good_func(h, 1);
print(g);
