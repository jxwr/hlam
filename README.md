## Hlam

To be a simple typed lambda calculus interpreter

### Simple Type Checker:

```
a = 1;
true;
1 + 2;
true || false && true;
if (true || false) then 1 + 2 else 100;
1 + 2 * 3 + 4 / 5 - 6;
1 + true;
20 & 10;
true + false;
true && false;
true && 1;
(\n : Int . n * n + 1) 100;
(\n : Int . n) false;
(\x : Bool . x) true;
(\x : Bool . x) if false then true else false;
a = (\x : Bool . x);
a true;
if (a true) then true else (a true);
id = (\x : Bool . x);
id false;
b = (\x : Bool . x);
b true;
b false true;
(\x:Bool . \x:Bool . x) true false true false;
1;
```

```
=======> test.f <=======
a = 1
true
(1+2)
(true||(false&&true))
(if (true||false) then (1+2) else 100)
(((1+(2*3))+(4/5))-6)
TypeError: '+' need two IntT operands, Int and Bool given
(20&10)
TypeError: '+' need two IntT operands, Bool and Bool given
(true&&false)
TypeError: '&&' need two BoolT operands, Bool and Int given
((\n : Int -> ((n*n)+1)) 100)
TypeError: function (\n : Int -> n) need Int typed argument, but Bool given 
((\x : Bool -> x) true)
((\x : Bool -> x) (if false then true else false))
a = (\x : Bool -> x)
(a true)
(if (a true) then true else (a true))
id = (\x : Bool -> x)
(id false)
b = (\x : Bool -> x)
(b true)
TypeError: (b false) is not a function
TypeError: (((\x : Bool -> (\x : Bool -> x)) true) false) is not a function
1
```
