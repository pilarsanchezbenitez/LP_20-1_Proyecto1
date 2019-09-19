module BAE where

    import ParseBAE
    import Data.List

  -- | BAE. Tipo que representa el conjunto de expresiones Aritmetico-Booleanas. 
    data BAE = V Name 
             | N Int 
             | B Bool
             | Suc BAE
             | Pre BAE
             | Plus BAE BAE
             | Prod BAE BAE
             | Neg BAE
             | Conj BAE BAE
             | Disy BAE BAE
             | Gt BAE BAE
             | Lt BAE BAE
             | Equi BAE BAE
             | Ift BAE BAE BAE
             | LetE Name BAE BAE
  
  -- | Sust. Tipo que representa una sustitución de las expresiones Aritmetico-Booleanas.           
    type Sust = (Name, BAE)
  -- | Ctx. Tipo que represeta al contexto para la verificacion de tipos.
    type Ctx = [(Name, Type)]
          
  -- | se instancia la clase Show para poder especificar como queremos que se impriman las
  -- | expresiones Aritmetico-Booleanas. 
    instance Show BAE where
      show (V n) = n
      show (N x) = "num["++show x++"]"
      show (B b) = "bool["++show b++"]"
      show (Suc e) = "suc("++show e++")"
      show (Pre e) = "pred("++show e++")"
      show (Plus e1 e2) = "plus("++show e1++","++show e2++")"
      show (Prod e1 e2) = "prod("++show e1++","++show e2++")"
      show (Neg e) = "neg("++show e++")"
      show (Conj e1 e2) = "and("++show e1++","++show e2++")"
      show (Disy e1 e2) = "or("++show e1++","++show e2++")"
      show (Gt e1 e2) = "gt("++show e1++","++show e2++")"
      show (Lt e1 e2) = "lt("++show e1++","++show e2++")"
      show (Equi e1 e2) = "eq("++show e1++","++show e2++")"
      show (Ift e1 e2 e3) = "if("++show e1++","++show e2++","++show e3++")"
      show (LetE x e1 e2) = "let("++show e1++","++x++"."++show e2++")"

  -- | transform. funcion que dada una declaracion devuelve su expresion Aritmetico-Booleana correspondiente
    transform_bae :: Stmt -> BAE
    transform_bae (Var n) = V n
    transform_bae (Num x) = N (fromIntegral x)
    transform_bae (BoolCt b) = B b
    transform_bae (ABUnary Not e) = Neg (transform_bae e)
    transform_bae (ABUnary Succ e) = Suc (transform_bae e)
    transform_bae (ABUnary Pred e) = Pre (transform_bae e)
    transform_bae (ABBinary And e1 e2) = Conj (transform_bae e1) (transform_bae e2)
    transform_bae (ABBinary Or e1 e2) = Disy (transform_bae e1) (transform_bae e2)
    transform_bae (ABBinary Add e1 e2) = Plus (transform_bae e1) (transform_bae e2)
    transform_bae (ABBinary Times e1 e2) = Prod (transform_bae e1) (transform_bae e2)
    transform_bae (ABRel GrT e1 e2) = Gt (transform_bae e1) (transform_bae e2)
    transform_bae (ABRel LowT e1 e2) = Lt (transform_bae e1) (transform_bae e2)
    transform_bae (ABRel Equ e1 e2) = Equi (transform_bae e1) (transform_bae e2)
    transform_bae (If e1 e2 e3) = Ift (transform_bae e1) (transform_bae e2) (transform_bae e3)
    transform_bae (Let x e1 e2) = LetE x (transform_bae e1) (transform_bae e2)
--Transform recibira una declaración y nos devolvera una tupla, con un BAE y el tipo del que será el BAE
--Notemos que utiliza como función auxiliar a Transform_bae, ese hara la evaluacion de cada caso para cada parte de la semantica de BAE
    transform :: StmtT -> (BAE,Type)
    transform (Typed s t) = (transform_bae s,t) 
    
  -- | fv. funcion que devuelve el conjunto de variables libres de una expresion Aritmetico-Booleanas.
  -- Queremos saber las variables libres que hay en cada expresion
  -- En cada operando se utiliza la recursion, donde nuestros casos base son los numeros, las variables y los booleanos
    fv :: BAE -> [Name]
    fv e = case e of 
        N n -> []
        V x -> [x]
        B b -> []
        Suc e -> fv e
        Pre e -> fv e
        Plus e1 e2 -> fv e1 `union` fv e2
        Prod e1 e2 -> fv e1 `union` fv e2
        Neg e1 -> fv e1
        Conj e1 e2 -> fv e1 `union` fv e2
        Disy e1 e2 -> fv e1 `union` fv e2
        Gt e1 e2 -> fv e1 `union` fv e2
        Lt e1 e2 -> fv e1 `union` fv e2
        Equi e1 e2 -> fv e1 `union` fv e2
        Ift e1 e2 e3 -> (fv e1 `union` fv e2) `union` fv e3
        LetE x e1 e2 -> fv e1 `union` (fv e2 \\[x]) 
  
  -- | substitution. funcion que devuelve la expresion Aritmetico-Booleanas resultante de aplicar
  -- | una sustitucion a la misma. 
  -- La funcion de substitucion en algunos cass tiene que hacer uso de las fv para que la sustitucion se haga correctamente
    substitution :: BAE -> Sust -> BAE
    substitution e s@(v, e') = case e of
                V x -> if x == v then e' else (V x)
                N n  -> N n
                B b -> B b
                Suc e -> Suc (substitution e s) 
                Pre e -> Pre(substitution e s)
                Plus e1 e2 -> Plus (substitution e1 s) (substitution e2 s)
                Prod e1 e2 -> Prod (substitution e1 s) (substitution e2 s)
                Neg e1 -> Neg (substitution e1 s)
                Conj e1 e2 -> Conj (substitution e1 s) (substitution e2 s)
                Disy e1 e2 -> Disy (substitution e1 s) (substitution e2 s)
                Gt e1 e2 -> Gt (substitution e1 s) (substitution e2 s)
                Lt e1 e2 -> Lt (substitution e1 s) (substitution e2 s)
                Equi e1 e2 -> Equi (substitution e1 s) (substitution e2 s)
                Ift e1 e2 e3 -> Ift (substitution e1 s) (substitution e2 s) (substitution e3 s)  
                LetE x e1 e2 -> LetE x (substitution e1 s) e4 where
                                          e4 = if not(x `elem`([v])`union` (fv e'))
                                              then substitution e2 s
                                              else error "error"
  
  -- | eval1. funcion que devuelve un paso en la evalucion de una expresion 
  -- | Aritmetico-Booleana a evaluar.
  -- Es un metodo auxiliar para elas, donde observa en cada tipo y en cada caso la evaluacion de cada operador
    eval1 :: BAE -> BAE
    eval1 e = case e of
      (N n) -> N n
      (B b) -> B b
      (V x) -> V x
      (Suc (N n)) -> N (n+1)
      (Suc e1) -> Suc (eval1 e1)
      (Pre (N 0)) -> N 0
      (Pre (N n)) -> N (n-1)
      (Pre e1) -> Pre (eval1 e1)
      (Plus (N n)(N m)) -> N (n+m)
      (Plus e1 e2) -> Plus (eval1 e1) (eval1 e2)
      (Prod (N n)(N m)) -> N (n*m)
      (Prod e1 e2) -> Prod (eval1 e1) (eval1 e2)
      (Neg (B True)) -> B (False)
      (Neg (B False)) -> B (True)
      (Neg e1) -> Neg (eval1 e1)
      (Conj (B True) (B True)) -> B (True)
      (Conj e1 e2) -> Conj (eval1 e1) (eval1 e2)
      (Disy (B False) (B False)) -> B (False)
      (Disy e1 e2) -> Disy (eval1 e1) (eval1 e2)
      (Gt (N n)(N m)) -> if (n < m) then B (True) else B (False)
      (Gt e1 e2) -> Gt (eval1 e1)(eval1 e2)
      (Lt (N n)(N m)) -> if (n > m) then B (True) else B (False)
      (Lt e1 e2) -> Lt (eval1 e1)(eval1 e2)
      (Equi (N n)(N m)) -> if (n == m) then B (True) else B (False)
      (Equi e1 e2) -> Equi (eval1 e1)(eval1 e2)
      (Ift (B True) e1 e2) -> eval1 e1
      (Ift (B False) e1 e2) -> eval1 e2
      (Ift e3 e1 e2)-> Ift (eval1 e3) e1 e2
      (LetE (x) (N n) e2) ->  substitution e2 (x, (N n))
      (LetE (x) (B b) e2) -> substitution e2 (x, (B b))
      (LetE (x) e1 e2) -> LetE (x) (eval1 e1) e2

  -- | evals. funcion que devuelve 0 o mas pasos de la evaluacion de una expresion
  -- | Aritmetico-Booleana.
  -- Este es una funcion auxiliar para eval donde nos ayuda a realizar la recursion de los casos base que son los números y los booleanos, y hace la composicionn de funciones para evals con eval1
    evals :: BAE -> BAE
    evals (N n) = N n
    evals (B b) = B b
    evals (V x) = V x
    evals e = evals $ eval1 e

  -- | isValue. funcion que determina si una expresion Aritmetico-Booleana es valor.
  --esta funcion nos ayudara para ver si la funcion la podemos evaluar, ya que debe cumplir con la especificacion de que cada operando recibe solo un booleano o un número y nos regresa lo mismo
    isValue :: BAE -> Bool 
    isValue e = case e of
      N n -> True
      B b -> True
      _ -> False 

  -- | eval. funcion que devuelve el valor de la evaluacion de una formula Aritmetico-Booleana.
    eval :: BAE -> BAE
    eval e = if isValue (evals e) then evals e else error "error"

-- vt solo nos dira si el operando es del tipo que debe devolver
    vt :: Ctx -> BAE -> Type -> Bool
    vt c (N n) NAT = True
    vt c (B b) BOOL = True
    vt c (V v) t = (v,t) `elem` c
    vt c (Suc e) NAT = vt c e NAT
    vt c (Pre e) NAT = vt c e NAT 
    vt c (Plus e1 e2) NAT = vt c e1 NAT && vt c e2 NAT
    vt c (Prod e1 e2) NAT = vt c e1 NAT && vt c e2 NAT
    vt c (Neg e1) BOOL = vt c e1 BOOL
    vt c (Conj e1 e2) BOOL = vt c e1 BOOL && vt c e2 BOOL
    vt c (Disy e1 e2) BOOL = vt c e1 BOOL && vt c e2 BOOL
    vt c (Lt e1 e2) BOOL = vt c e1 NAT && vt c e2 NAT
    vt c (Gt e1 e2) BOOL = vt c e1 NAT && vt c e2 NAT
    vt c (Equi e1 e2) BOOL = vt c e1 NAT && vt c e2 NAT
    vt c (Ift e1 e2 e3) t = vt c e1 BOOL && vt c e2 t && vt c e3 t   
    vt c (LetE x e1 e2) t = case (vt c e1 NAT) of
      True -> vt (c `union` [(x,NAT)]) e2 t
      False -> case (vt c e1 BOOL) of
        True -> vt (c `union`[(x,BOOL)]) e2 t
        False -> False
    vt _ _ _ = False

