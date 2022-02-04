def x(i = 1): 
    if i == 1 or i == 2:
        return 1;
    else:
        return x(i - 1) + x(i-2);;;
a = x(5);
b = x();

