## My haskell exercise book

### Bool Typed:

```
true;

a = true;

if false then true else false;

a true false;

id = (\x : Bool . x);

id false;

id id; /* bug? */

(\x:Bool . \x:Bool . x) true false;
```
