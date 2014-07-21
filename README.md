## Hlam

To be a simple typed lambda calculus interpreter

### Simple Type Checker:

```
true;
1 + 2 * 3;
1 + true;
true + false;
true && false;
true && 1;
(\n : Int . n) 100;
(\n : Int . n) false;
(\x : Bool . x) true;
(\x : Bool . x) if false then true else false;
a = (\x : Bool . x);
a true;
id = (\x : Bool . x);
id false;
b = (\x : Bool . x);
b true;
b false true;
(\x:Bool . \x:Bool . x) true false true false;
```

```
=======> test.f <=======
true
(1+(2*3))
TypeError: '+' need two IntT operands, Int and Bool given
TypeError: '+' need two IntT operands, Bool and Bool given
(true&&false)
TypeError: '&&' need two BoolT operands, Bool and Int given
((\n : Int -> n) 100)
TypeError: function (\n : Int -> n) need Int typed argument, but Bool given 
((\x : Bool -> x) true)
((\x : Bool -> x) (if false then true else false))
a = (\x : Bool -> x)
(a true)
id = (\x : Bool -> x)
(id false)
b = (\x : Bool -> x)
(b true)
TypeError: (b false) is not a function
TypeError: (((\x : Bool -> (\x : Bool -> x)) true) false) is not a function
```
