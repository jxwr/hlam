
true;

1 + 2 * 3;

1 + true;

true + false;

true && false;

true && 1;

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

