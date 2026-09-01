import Parsing
import Control.Applicative
-- 
-- expr → term (’+’ expr | ’-’ expr | ε)
-- term → factor (’*’ term | ’/’ term | ε)



expr :: Parser Int
expr = do t <- term
          (do symbol "+"
              e <- expr
              return (t + e))
            <|> (do symbol "-"
                    e <- expr
                    return (t - e))
                 <|> return t

term :: Parser Int
term = do f <- factor
          (do symbol "*"
              t <- term
              return (f*t))
              <|> (do symbol "/"
                      t <- term
                      return (f `div` t))
                  <|> return f

factor :: Parser Int
factor = (do symbol "("
             e <- expr
             symbol ")"
             return e)
         <|> natural

trans:: Parser a -> Parser a
trans p = (do symbol "["
              e <- p
              symbol "]"
              return e
            <|> p)

expr2 :: Parser Expr
expr2 = do t <- term2
           (do symbol "+"
               e <- expr2
               return (BinOp Add t e))
            <|> (do symbol "-"
                    e <- expr2
                    return (BinOp Min t e))
            <|> return t

term2 :: Parser Expr
term2 = do f <- factor2
           (do symbol "*"
               t <- term2
               return (BinOp Mul f t))
              <|> (do symbol "/"
                      t <- term2
                      return (BinOp Div f t))
                  <|> return f

factor2 :: Parser Expr
factor2 = (do symbol "("
              e <- expr2
              symbol ")"
              return e)
          <|> do e <- natural
                 return (Num e)


hasktype :: Parser Hasktype
hasktype = do string "Int"
              symbol "->"
              e <- hasktype
              return (DInt : e)
            <|> do string "Char"
                   symbol "->"
                   e <- hasktype
                   return (DChar : e)
                 <|> do string "Float"
                        symbol "->" 
                        e <- hasktype
                        return (DFloat : e)
                      <|> do    string "Int"
                                e <- hasktype
                                return (DInt : e)
                           <|> do string "Char"
                                  e <- hasktype
                                  return (DChar : e)
                                <|> do string "Float"
                                       e <- hasktype
                                       return (DFloat : e)
                                     <|> return []

tipo :: Parser Basetype
tipo = do string "Int"
          return DInt
        <|> do string "Float"
               return DFloat
             <|> do string "Char"
                    return DChar


type2hask :: Parser Hasktype
type2hask = sepBy tipo (symbol "->")

tipo2 :: Parser Basetype
tipo2 = do e <- natural
           return DInt
         <|> do symbol "'"
                e <- identifier
                symbol "'"
                return DChar


list2hask :: Parser Hasktype
list2hask = do symbol "["
               x <- sepBy tipo2 (symbol ",")
               symbol "]"
               return x

--GRAMATICA
-- t ==>  v ( '->' t | e)
-- v ==> 'Int' | 'Char' | 'Float' | '(' t ')'


tipo3 :: Parser Hasktype2
tipo3 = do string "Int"
           return DInt2
         <|> do string "Float"
                return DFloat2
              <|> do string "Char"
                     return DChar2


fun2hask :: Parser Hasktype2
fun2hask = do e <- value
              (do symbol "->"
                  t <- fun2hask
                  return (Fun e t))
               <|> return e


value :: Parser Hasktype2
value = do e <- tipo3
           return e
        <|> do symbol "("
               e <- fun2hask
               symbol ")"
               return e