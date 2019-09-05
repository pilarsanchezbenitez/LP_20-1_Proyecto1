module BAE where
    import ParseBAE
    import Data.List

    type Sust = (Name, BAE)
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
    transform :: Stmt -> BAE
    transform (Var n) = V n
    transform (Num x) = N (fromIntegral x)
    transform (BoolCt b) = B b
    transform (ABUnary Not e) = Neg (transform e)
    transform (ABUnary Succ e) = Suc (transform e)
    transform (ABUnary Pred e) = Pre (transform e)
    transform (ABBinary And e1 e2) = Conj (transform e1) (transform e2)
    transform (ABBinary Or e1 e2) = Disy (transform e1) (transform e2)
    transform (ABBinary Add e1 e2) = Plus (transform e1) (transform e2)
    transform (ABBinary Times e1 e2) = Prod (transform e1) (transform e2)
    transform (ABRel GrT e1 e2) = Gt (transform e1) (transform e2)
    transform (ABRel LowT e1 e2) = Lt (transform e1) (transform e2)
    transform (ABRel Equ e1 e2) = Equi (transform e1) (transform e2)
    transform (If e1 e2 e3) = Ift (transform e1) (transform e2) (transform e3)
    transform (Let x e1 e2) = LetE x (transform e1) (transform e2)      


  -- | fv. funcion que devuelve el conjunto de variables libres de una expresion Aritmetico-Booleanas.
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
    eval1 :: BAE -> BAE
    eval1 e = case e of
      Suc (N n) -> N (n+1)
    --eval1 e = error $ "Execution Error: Locked state."
  
  -- | evals. funcion que devuelve 0 o mas pasos de la evaluacion de una expresion
  -- | Aritmetico-Booleana.
    evals :: BAE -> BAE
    evals (N n)= N n
    evals (B b)= B b
    evals (V x)= V x
    evals e = evals $ eval1 e

  -- | isValue. funcion que determina si una expresion Aritmetico-Booleana es valor.
    isValue :: BAE -> Bool 
    isValue e = case e of
      N n -> True
      B b -> True
      _ -> False 

  -- | eval. funcion que devuelve el valor de la evaluacion de una formula Aritmetico-Booleana.
    eval :: BAE -> BAE
    eval e = if isValue (evals e) then evals e else error "error"

    data Tipo = NAT | BOOL

    type Ctx = [(Name,Tipo)]

    vt :: Ctx -> BAE -> Tipo -> Bool
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
    --vt c (LetE x e1 e2) t = 
