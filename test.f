/* Examples for testing */

true;
if false then true else false; 
if true then false else false;

0; 
succ (pred 0);
iszero (pred (succ (succ 0))); 

if 0 then 1 else 2;
succ false;

lambda x. x;
(lambda x. x) (lambda x. x x); 

(lambda x. x) 26;
if (lambda x. x) true then 1 else 2;

(lambda x. if x then true else false) 0;
(lambda x. if x then true else false) true;
(lambda x. if x then true else false) false;

(lambda x. x (lambda x. x)) 1;

(lambda x. lambda y. y x) 26 (lambda z. if iszero z then 20 else 23);
