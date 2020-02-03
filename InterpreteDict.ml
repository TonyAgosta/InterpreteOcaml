type ide = string;;

type exp = 
  | Eint of int 
  | Ebool of bool 
  | Estring of string
  | Den of ide 
  | Prod of exp * exp 
  | Sum of exp * exp 
  | Diff of exp * exp 
  | Eq of exp * exp 
  | Minus of exp 
  | IsZero of exp 
  | Or of exp * exp 
  | And of exp * exp 
  | Not of exp 
  | Ifthenelse of exp * exp * exp 
  | Let of ide * exp * exp 
  | Fun of ide * exp 
  | FunCall of exp * exp 
  | Letrec of ide * exp * exp
  
  (*Dizionario*)
  | MyFun of ide * ide * exp
  | MyFunCall of exp * exp * exp
  | Dict of mydictionary
  | Insert of ide * exp * exp
  | Delete of exp * ide
  | Has_key of ide * exp
  | Iterate of exp * exp
  | Fold of exp * exp
  | Filter of ide list * exp
  and mydictionary= Empty| Pair of ide * exp * mydictionary;;

(*ambiente polimorfo*)
type 't env = ide -> 't;;
let emptyenv (v : 't) = function x -> v;;
let applyenv (r : 't env) (i : ide) = r i;;
let bind (r : 't env) (i : ide) (v : 't) = function x -> if x = i then v else applyenv r x;;

(*tipi esprimibili*)
type evT = 
  | Int of int 
  | Bool of bool 
  | String of string
  | Unbound 
  | FunVal of evFun 
  | RecFunVal of ide * evFun
  | MyFunVal of ide * ide * exp * evT env
  | Dictval of (ide * evT) list
and evFun = ide * exp * evT env


(*rts*)
(*type checking*)
let typecheck (s : string) (v : evT) : bool = match s with
	| "int" -> (match v with
		| Int(_) -> true
		|_ -> false)
	| "bool" -> (match v with
		| Bool(_) -> true 
		|_ -> false)
  (*| "string" -> (match v with
    | String(_) -> true
    | _-> false)*)
  | _ -> failwith("not a valid type");;

(*funzioni primitive*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
    (Int(n),Int(u)) -> Int(n*u)
    |(_,_)-> failwith("typecheckerror"))
	else failwith("Type error");;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
    (Int(n),Int(u)) -> Int(n+u)
    |(_,_)-> failwith("typecheckerror")) 
	else failwith("Type error");;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
    (Int(n),Int(u)) -> Int(n-u)
    |(_,_)-> failwith("typecheckerror"))
	else failwith("Type error");;

let eq x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
    (Int(n),Int(u)) -> Bool(n=u)
    |(_,_)-> failwith("typecheckerror") )
	else failwith("Type error");;

let minus x = if (typecheck "int" x) 
	then (match x with
       Int(n) -> Int(-n)
       |(_)-> failwith("typecheckerror"))
	else failwith("Type error");;

let iszero x = if (typecheck "int" x)
	then (match x with
    Int(n) -> Bool(n=0)
    |(_) -> failwith("typecheckerror"))
	else failwith("Type error");;

let vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
    (Bool(b),Bool(e)) -> (Bool(b||e))
    |(_,_)-> failwith("typecheckerror") )
	else failwith("Type error");;

let et x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
    (Bool(b),Bool(e)) -> Bool(b&&e)
    |(_,_)-> failwith("typecheckerror") )
	else failwith("Type error");;

let non x = if (typecheck "bool" x)
	then (match x with
		Bool(true) -> Bool(false) |
    Bool(false) -> Bool(true) |
    (_) -> failwith("typecheckerror"))
	else failwith("Type error");;

(*Funzioni ricorsive per poter eseguire operazioni sul dizionario*)

(*Funzione ricorsiva utilizzato per vedere se una chiave é presente nel dizionario*)
let rec isPresent value diz=
  (match diz with
  |[]-> false
  |(k1,v)::ds-> if (value=k1) then true else isPresent value ds
  );;

(*Funzione ricorsiva utilizzata per vedere se un elemento é presente all'interno di una lista*)
let rec isPresent1 value diz=
  (match diz with
  |[]-> false
  |(d::ds)-> if (value=d) then true else isPresent1 value ds
  );;

(*Funzione ricorsiva utilizzata per rimuovere dal dizionario la coppia chiave-valore identificata da "value"*)
let rec removeKey value diz=
  (match diz with
  |[]->[]
  |(k1,v)::ds -> if (value=k1) then ds else (k1,v)::removeKey value ds
  );;

(*Funzione ricorsiva utilizzata per ottenere il dizionario senza le coppie chiave volore identificate dalla lista di chiavi passata come argomento*)
let rec recFilter listkey diz=
  (match diz with
  []->[]
  |(k1,v)::ds-> if (isPresent1 k1 listkey) then recFilter listkey ds
                else (k1,v)::(recFilter listkey ds)  
  );;

(*interprete*)
let rec eval (e : exp) (r : evT env) : evT = match e with
	Eint n -> Int n |
  Ebool b -> Bool b |
  Estring s -> String s |
	IsZero a -> iszero (eval a r) |
	Den i -> applyenv r i |
	Eq(a, b) -> eq (eval a r) (eval b r) |
	Prod(a, b) -> prod (eval a r) (eval b r) |
	Sum(a, b) -> sum (eval a r) (eval b r) |
	Diff(a, b) -> diff (eval a r) (eval b r) |
	Minus a -> minus (eval a r) |
	And(a, b) -> et (eval a r) (eval b r) |
	Or(a, b) -> vel (eval a r) (eval b r) |
	Not a -> non (eval a r) |
	Ifthenelse(a, b, c) -> 
		let g = (eval a r) in
			if (typecheck "bool" g) 
				then (if g = Bool(true) then (eval b r) else (eval c r))
				else failwith ("nonboolean guard") |
	Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r)) |
  Fun(i, a) -> FunVal(i, a, r) |
  MyFun(valore,i,a) ->MyFunVal(valore,i,a,r) |
	FunCall(f, eArg) -> 
		let fClosure = (eval f r) in
			(match fClosure with
				FunVal(arg, fBody, fDecEnv) -> 
					eval fBody (bind fDecEnv arg (eval eArg r)) |
				RecFunVal(g, (arg, fBody, fDecEnv)) -> 
					let aVal = (eval eArg r) in
						let rEnv = (bind fDecEnv g fClosure) in
							let aEnv = (bind rEnv arg aVal) in
								eval fBody aEnv |
				_ -> failwith("non functional value")) |
  Letrec(f, funDef, letBody) ->
        		(match funDef with
            		Fun(i, fBody) -> let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) in
                         			                eval letBody r1 |
                _ -> failwith("non functional def")) |
  
  MyFunCall (funct,valore1,valore2)->
      let fClosure = (eval funct r) in
          (match fClosure with
            | MyFunVal(arg1, arg2, fBody, fDecEnv) ->
             let rEnv = (bind fDecEnv arg1 (eval valore1 r)) in
                let aEnv = (bind rEnv arg2 (eval valore2 r)) in
                      eval fBody aEnv 
            | _ -> failwith("non functional value")) |
  
  (*Interprete sul dizionario*)
  Dict(d)-> Dictval(evalMydictionary d r)|
  
  Insert(k, s, d) -> 
    (match eval d r with
    |Dictval(dict) -> if(isPresent k dict) then failwith("chiave gia presente")
                      else (match dict with
                      | [] -> Dictval((k,(eval s r))::dict)
                      | (k1,v)::ds -> Dictval((k,(eval s r))::dict)
                      )
    |_-> failwith("Error insert") 
    ) |
  Delete(d,k) ->
    (match eval d r with
    |Dictval(dict)-> Dictval(removeKey k dict)
    |_ -> failwith("error delete")
    ) |
  Has_key(k,d) -> 
    (match eval d r with
    |Dictval(dict)-> if(isPresent k dict) then Bool(true)
                      else Bool(false)
    |_ -> failwith("error has_key")
    ) |
  Filter(kl,d) ->
    (match eval d r with
    |Dictval(dict)-> Dictval(recFilter kl dict)
    |_ -> failwith("error filter")
    ) |
  Iterate(f,d) ->
    (match d with
    |Dict dict ->Dictval(iterate dict r f)
    |_-> failwith("error in Iterate")
    ) |
  Fold(f,d)->
    (match eval d r with
    |Dictval(dict) -> (match f with
                      |MyFun(accumulatore,ii,op)-> 
                            (match op with
                              |Sum(_,_)-> (accuFold (Int(0)) f dict r)
                              |Diff(_,_)-> (accuFold (Int(0)) f dict r)
                              |Prod(_,_)-> (accuFold (Int(1)) f dict r)
                              |_ -> failwith("Nessuna operazione possibile")
                            )
                      |_-> failwith("errore match Fold"))
    |_->failwith ("error fold")
    )
  and evalMydictionary (dict: mydictionary)(r: evT env)=
                          (match dict with
                          |Empty->[]
                          |Pair(k,v,d)-> (k,eval v r):: evalMydictionary d r)
 
  and iterate (dict : mydictionary)(r1: evT env)(funct:exp) =
                (match dict with
                | Empty->[]
                | Pair(k,v,d)-> (k,eval (FunCall (funct ,v)) r1)::(iterate d r1 funct)
                )
  and accuFold (valore :evT)(funct:exp)(dict:(ide*evT) list) (r1:evT env) :evT=
              (match dict with
              | []->valore
              | (k,v)::d-> (match(valore,v) with
                            | Int(x),Int(y) -> accuFold(eval(MyFunCall(funct,Eint(x),Eint(y))) r1) funct d r1
                            | _ -> failwith("error inside fold")
                            )
              )
;;
(* =============================  TESTS  ============================= *)

(* basico: no let *)
let env0 = emptyenv Unbound;;

(*Creo il dizionario con la prima coppia chiave-valore*)

let d = Dict(Pair("mele", Eint(430),Empty));;
let diz = eval d env0;;

(*Test su Insert: inserisco diverse coppie chiave-valore per avere piu elementi su cui operare*)

let d1 = Insert("banane", Eint(312), d);;
let d2 = Insert("arance", Eint(525), d1)
let d3 = Insert("pere", Eint(217), d2);;
let d4 = Insert("kiwi", Eint(300) ,d3);;
let diz1 = eval d4 env0;;

(*Test su Delete: rimuovo una coppia chiave-valore dal dizionario*)

let d5 = Delete(d4, "pere");;
let diz2 = eval d5 env0;;

(*Test su Has_key: prima testo un elemento presente nel dizionario,poi un elemento che non fa parte del dizionario*)

let d6 = Has_key("kiwi",d5);;
let bool1 = eval d6 env0;;

let d7 = Has_key("meloni",d5);;
let bool2 = eval d7 env0;;

(*Test su Iterate sul dizionario iniaziale*)

let d8 = Iterate((Fun("x", Sum(Den "x", Eint(1)))), d);;

let diz3 = eval d8 env0;;

(*Test sulla Fold*)

let d9 = Fold(MyFun("x","y",Sum(Den "x", Eint(1))),d5);;
let diz4 = eval d9 env0;;

(*Test su Filter: elimino dal dizionario le coppie chiave-valore che hanno come chiave
le chiavi presenti nella lista passata come argomento*)

let d10= Filter(["banane"; "pere"], d5);;
let diz5= eval d10 env0;;