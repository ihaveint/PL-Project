def x(l=[1,5,8,9], lt=[2,4,7,10]):
    i = 0;
    ret = [];
    for t in [0,1,2,3,4,5,6,7]:
        if i == 4:
            ret = ret + [lt[t-i]];
        else:
            if t-i == 4:
                ret = ret + [l[i]];
                i = i + 1;
            else:
                if l[i] < lt[t-i]:
                    ret = ret + [l[i]];
                    i = i + 1;
                else:
                    ret = ret + [lt[t-i]];
                ;
            ;
        ;
    ;
    return ret;
;


print(x(), x([1,2,3,4]), x([2,4,6,8], [1,3,5,7]));

