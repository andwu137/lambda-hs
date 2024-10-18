module Language.Lambda.Targets.Interpreter.SymbolTable (
    defaultSymbolTable,
    if',
    printId,
    myPrint,
    myPutStrLn,
    myShow,
    showAbs,
    add,
    mul,
    div,
    sub,
) where

import Control.Monad (join, (<=<))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Functor (($>), (<&>))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Language.Lambda.Expr as E
import qualified Language.Lambda.Targets.Interpreter.Core as I
import qualified Language.Lambda.Targets.Interpreter.Reduction as I
import Prelude hiding (div, lookup)
import qualified Prelude hiding (lookup)

defaultSymbolTable :: I.SymbolTable IO
defaultSymbolTable =
    M.fromList
        [ ("print", I.Builtin myPrint)
        , ("putStrLn", I.Builtin myPutStrLn)
        , ("show", I.Builtin $ fmap (I.Const . E.String) . myShow)
        , ("printId", I.Builtin printId)
        , ("+", I.Builtin $ curry2 add)
        , ("-", I.Builtin $ curry2 sub)
        , ("*", I.Builtin $ curry2 mul)
        , ("/", I.Builtin $ curry2 div)
        , ("if", I.Builtin $ curry3 if')
        , ("<", I.Builtin $ curry2 $ cmp (<))
        , ("<=", I.Builtin $ curry2 $ cmp (<=))
        , (">", I.Builtin $ curry2 $ cmp (>))
        , (">=", I.Builtin $ curry2 $ cmp (>=))
        , ("==", I.Builtin $ curry2 $ cmp (==))
        , ("/=", I.Builtin $ curry2 $ cmp (/=))
        ]
  where
    cmp f x y = do
        x' <- I.eval x
        y' <- I.eval y
        pure $ I.Const $ E.Bool (f x' y')

curry2 :: (Monad m) => (E.Expr -> E.Expr -> I.InterT m (I.Output m)) -> E.Expr -> I.InterT m (I.Output m)
curry2 f = pure . I.Builtin . f

curry3 :: (Monad m) => (E.Expr -> E.Expr -> E.Expr -> I.InterT m (I.Output m)) -> E.Expr -> I.InterT m (I.Output m)
curry3 f x = pure $ I.Builtin $ \y -> pure $ I.Builtin $ \z -> f x y z

cast = undefined -- TODO: cast function

if' :: E.Expr -> E.Expr -> E.Expr -> I.InterT IO (I.Output IO)
if' b x y = do
    b' <- I.eval b
    case b' of
        (E.Bool p) -> pure $ if p then I.Const x else I.Const y
        _ -> I.throwE "if: Expected boolean for condition"

printId :: E.Expr -> I.InterT IO (I.Output IO)
printId x = myPrint x $> I.Const x

myPrint :: E.Expr -> I.InterT IO (I.Output IO)
myPrint = myPutStrLn . E.String <=< myShow

myPutStrLn :: E.Expr -> I.InterT IO (I.Output IO)
myPutStrLn = \case
    E.String s -> lift (putStrLn s) $> I.Const E.Unit
    x -> I.throwE $ "Input was not a string: " <> T.pack (show x)

myShow :: E.Expr -> I.InterT IO String
myShow =
    \case
        E.Undefined -> I.eval E.Undefined >>= myShow
        E.Strict e -> myShow e
        E.Bool b -> pure $ show b
        E.Unit -> pure "Unit"
        E.Z x -> pure $ show x
        E.R x -> pure $ show x
        E.String x -> pure x
        E.Ident i ->
            I.lookup i >>= \case
                I.Builtin _ -> pure (T.unpack i)
                I.Const x -> myShow x
        a@(E.Abs{}) -> showAbs a
        a@(E.App{}) -> myShow =<< I.eval a
        o@(E.Op{}) -> myShow =<< I.eval o

showAbs :: E.Expr -> I.InterT IO [Char]
showAbs = \case
    E.Undefined -> I.eval E.Undefined >>= showAbs
    E.Strict e -> showAbs e
    E.Bool b -> pure $ show b
    E.Unit -> pure $ show ()
    E.Z x -> pure $ show x
    E.R x -> pure $ show x
    E.String x -> pure x
    E.Ident i -> pure $ T.unpack i -- TODO: Fix ident is an operator issue
    E.Abs f b -> showAbs b <&> \x -> concat ["λ", T.unpack f, ". ", x]
    E.App ml mr -> do
        (\l r -> concat [showParens ml l, " ", showParens mr r])
            <$> showAbs ml
            <*> showAbs mr
    E.Op o ml mr ->
        (\l r -> concat [showParens ml l, " ", T.unpack o, " ", showParens mr r])
            <$> showAbs ml
            <*> showAbs mr

parens :: String -> String
parens x = concat ["(", x, ")"]

showParens :: E.Expr -> String -> String
showParens = \case
    E.Strict e -> showParens e
    E.Op{} -> parens
    E.App{} -> parens
    E.Abs{} -> parens
    E.Undefined -> id
    E.Bool{} -> id
    E.Unit -> id
    E.Z{} -> id
    E.R{} -> id
    E.String{} -> id
    E.Ident{} -> id

add :: E.Expr -> E.Expr -> I.InterT IO (I.Output IO)
add x y =
    join $ add' <$> I.eval x <*> I.eval y
  where
    add' = \cases
        (E.Z a) (E.Z b) -> pure $ I.Const $ E.Z (a + b)
        (E.R a) (E.R b) -> pure $ I.Const $ E.R (a + b)
        _ _ -> I.throwE "+: No matching function"

mul :: E.Expr -> E.Expr -> I.InterT IO (I.Output IO)
mul x y =
    join $
        mul' <$> I.eval x <*> I.eval y
  where
    mul' = \cases
        (E.Z a) (E.Z b) -> pure $ I.Const $ E.Z (a * b)
        (E.R a) (E.R b) -> pure $ I.Const $ E.R (a * b)
        _ _ -> I.throwE "*: No matching function "

div :: E.Expr -> E.Expr -> I.InterT IO (I.Output IO)
div x y =
    join $ div' <$> I.eval x <*> I.eval y
  where
    div' = \cases
        (E.Z a) (E.Z b) -> pure $ I.Const $ E.Z (a `Prelude.div` b)
        (E.R a) (E.R b) -> pure $ I.Const $ E.R (a / b)
        _ _ -> I.throwE "/: No matching function"

sub :: E.Expr -> E.Expr -> I.InterT IO (I.Output IO)
sub x y =
    join $ sub' <$> I.eval x <*> I.eval y
  where
    sub' = \cases
        (E.Z a) (E.Z b) -> pure $ I.Const $ E.Z (a - b)
        (E.R a) (E.R b) -> pure $ I.Const $ E.R (a - b)
        _ _ -> I.throwE "-: No matching function"
