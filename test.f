/* Examples for testing */

true;
if false then true else false; 
if true then true else false;

0; 
succ (pred 0);
iszero (pred (succ (succ 0))); 

lambda x:nat. x;

(lambda x:nat->bool. x) (lambda x:nat. iszero x); 

(lambda x:nat. x) 26;
if (lambda x:bool. x) true then 1 else 2;

(lambda x:bool. if x then true else false) true;
(lambda x:bool. if x then true else false) false;

(lambda x:nat. (lambda x:nat. iszero x) (pred x)) 1;

(lambda x:nat. lambda y:nat->nat. y x) 26 (lambda z:nat. if iszero z then 20 else 23);

{x=true, y=false};
{x=true, y=false}.x;

lambda x:{a:nat}. x.a;
(lambda x:{a:nat}. x.a) {a=1};
(lambda x:{a:nat}. x.a) {a=1, b=2};
(lambda x:{a:nat}. x.a) {b=2, a=1};

lambda x:{x:{x:bool}}. x.x.x;
(lambda x:{x:{x:bool}}. x.x.x) {x={x=true, y=false, z=3}, a=4, b=succ 0};

{a=(lambda x:bool. if x then true else false) true, b=pred (pred (succ (succ (pred 0)))), c=26};
((((lambda r:{c:nat, a:bool, b:nat}. if r.a then r.c else r.b))));
(lambda r:{c:nat, a:bool, b:nat}. if r.a then r.c else r.b) {a=(lambda x:bool. if x then true else false) true, b=pred (pred (succ (succ (pred 0)))), c=26};

lambda f:{a:nat, b:nat}->{a:nat}. lambda x:{a:nat, b:nat}. f x;
(lambda f:{a:nat, b:nat}->{a:nat}. lambda x:{a:nat, b:nat}. f x) (lambda y:{a:nat}. {a=y.a, z=26});
(lambda f:{a:nat, b:nat}->{a:nat}. lambda x:{a:nat, b:nat}. f x) (lambda y:{a:nat}. {a=y.a, z=26}) {a=20, b=23};
