/* Examples for testing */

true;
if false then true else false; 
if true then false else false;

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
