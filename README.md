## My haskell exercise book

### Simple Type Checker:

```
true;

(\x : Bool . x) true;

(\x : Bool . x) if false then true else false;

a = (\x : Bool . x);
a true;

id = (\x : Bool . x);
id false;

b = (\x : Bool . x);

b true;

b false true; /* error */

(\x:Bool . \x:Bool . x) true false true false;
```

```
=======> test.f <=======
PASS: true
PASS: ((\x : Bool -> x) true)
PASS: ((\x : Bool -> x) (if false then true else false))
PASS: a = (\x : Bool -> x)
PASS: (a true)
PASS: id = (\x : Bool -> x)
PASS: (id false)
PASS: a = (\x : Bool -> x)
PASS: (a true)
FAIL: TypeError: (a false) is not a function
```
