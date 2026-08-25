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