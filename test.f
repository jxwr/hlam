true;

(\x : Bool . x) true;

(\x : Bool . x) if false then true else false;

a = (\x : Bool . x);
a true;

id = (\x : Bool . x);
id false;

a = (\x : Bool . x);

a true;

a false true; /* error */

(\x:Bool . \x:Bool . x) true false true false;
