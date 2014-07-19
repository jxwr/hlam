true;
if false then true else false;
id = (\x : Bool . x);
id false;
id id;
(\x:Bool . \x:Bool . x) true false;
