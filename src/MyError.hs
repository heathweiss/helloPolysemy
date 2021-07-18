{-
This is my own module, that will test the interaction between the build in Error, and IO.

I will create a 'Name', that must be "Heath", else will be an Text error. Note that OverloadedStrings is included via Package.yaml

There is documentation at https://hackage.haskell.org/package/polysemy-1.5.0.0/docs/Polysemy-Internal.html which is Polysemy.Internal module.

See: https://github.com/KerfuffleV2/haskell-polysemy-test example 3 which Sandy modified.
This is what I am modelling my makeName program after.
-}
module MyError () where
import qualified Data.Text as T
import Polysemy (Member, Members, Sem, interpret, run, runM, intercept, makeSem_, reinterpret)
import Polysemy.State (State, get, modify, put, evalState)
import Polysemy.Error (Error, catch, throw, runError, fromEither)
import Polysemy.Trace (Trace(..), trace, traceToOutput, traceToIO, runTraceList, ignoreTrace)
import Polysemy.Input (Input)
import Polysemy.Output (runOutputSem, ignoreOutput)
import Polysemy.Embed (Embed(..),runEmbedded, embed)
import Data.Function ((&))

newtype Name = Name T.Text deriving (Show)
newtype Email = Email T.Text deriving Show

------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------- create a Name using a fx that returns a Sem monad

--Do I need to make my own interpretation, if I can use existing ones.
--See: https://github.com/KerfuffleV2/haskell-polysemy-test/pull/1
--data Name m a where
  --MakeName :: T.Text -> T.Text -> Name m NameText runTraceIO

makeNameWithTrace :: forall r. Members '[Trace] r => Sem r Name
makeNameWithTrace = do
  trace "trace msg in makeName"
  pure $ Name "Heath"
  

tracePrefix :: Member Trace r => Sem r a -> Sem r a
tracePrefix = intercept (\case
  Trace msg -> trace $ "log: " ++ msg)
  --Trace msg -> embed $ print $ "> " ++ msg)


{-Use the manual makeNameWithTrace fx to create a Name, and do use Trace effect.-}
makeNameWithTraceList :: IO Name
makeNameWithTraceList = do
 let (s,a) = makeNameWithTrace & runTraceList & run
 mapM_ print s
 return a

{-Use the manual makeNameWithTrace fx to create a Name, and do use Trace effect.
  Use the tracePrefix fx to prepend strings to the trace strings.
-}
makeNameWithTracePrefix :: IO Name
makeNameWithTracePrefix = do
 let (s,a) = makeNameWithTrace & tracePrefix  & runTraceList  & run
 mapM_ print s
 return a

------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------- create a name using a NameSem effect
data NameSem m a where
  MakeName :: T.Text -> NameSem m Name
  MakeSafeName :: T.Text -> NameSem m (Either T.Text Name) 
  
data EmailSem m a where
  MakeEmail :: Name -> EmailSem m Email

makeSem_ ''NameSem
makeSem_ ''EmailSem

--This is just a wrapper around 'makeName "Heath"' which can be eliminated
--by directly calling 'makeName "Heath"'
--Would only need this for multiple steps, such as error handling, using kvStore, etc.
createName :: Member NameSem r => Sem r Name
createName = 
  makeName "Heath"

--interpretName :: Member NameSem r => Sem (NameSem : r) a -> Sem r a 
--Do not put the  'Member NameSem r =>' in there, as it then has that effect, 
--which then has to be interpreted.
interpretName :: Sem (NameSem : r) a -> Sem r a 
interpretName = interpret $ \case
  MakeName nameString -> return $ Name nameString
  MakeSafeName nameString -> return $ newSafeName nameString


interpretEmail :: Sem (EmailSem : r) a -> Sem r a
interpretEmail = interpret $ \case
  MakeEmail (Name name) -> return $ Email (name <> "@hotmail.com")

runCreateName :: IO ()
runCreateName = do
  --print $ createName & interpretName & run
  --No need to use the createName fx, call makeName directly.
  print $ makeName "Heath" & interpretName & run
  return ()


----------------------------------------------------------------------------------------------------------------------------------------------------------
--Combine the use of the above 2 effects, NameSem and Trace, to make a Name with tracing.
makeEffectfulNameWithTrace :: forall r. Members '[Trace,NameSem] r => Sem r Name
makeEffectfulNameWithTrace = do
  trace "make name: Heath"
  makeName "Heath"

runCreateEffectfulNameAndTrace :: IO ()
runCreateEffectfulNameAndTrace = do
  let (s,a) = makeEffectfulNameWithTrace & interpretName & runTraceList & run
  mapM_ print s
  print a
  return ()

--instead of using print in IO, print using traceToIO interpretation.
runCreateEffectfulNameAndTraceWithoutIO :: IO Name
runCreateEffectfulNameAndTraceWithoutIO = 
  makeEffectfulNameWithTrace & interpretName & traceToIO  & runM
  
  ----------------------------------------------------------------------------------------------------------------------------------------------------------
--Now add error handling.
{- 
Will use the MakeSafeName constructor of NameSem 

Will be an error to create a name that is not Heath.

Will use a smart constructor to create and Either String Name

Will convert the Either value into an Error


-}
newSafeName :: T.Text -> Either T.Text Name
newSafeName name = 
  if name == "Heath" then Right $ Name name
    else
      Left "name was not Heath"


--the high level effects code
--All is does is call makeSafeName, so it is pointless, but would be req'd if it was more complex.
--Eg: createSafeNameWithError calls makeSafeName and binds it to fromEither to add the Error effect.
createSafeName :: Member NameSem r => T.Text -> Sem r (Either T.Text Name)
createSafeName nameAsText = do
  makeSafeName nameAsText


createSafeNameWithError :: Members '[NameSem, Error T.Text] r => T.Text -> Sem r Name
createSafeNameWithError nameAsText = do
  makeSafeName nameAsText >>= fromEither 


createNameAndEmail :: Members '[NameSem, EmailSem, Error T.Text] r => T.Text -> Sem r Email
createNameAndEmail nameAsText = do
  --nameE <- makeSafeName nameAsText
  --name <- fromEither nameE
  --or combine with bind operator, so it extracts if from the Either the way it would be done if working in Either Monad.
  --name <- makeSafeName nameAsText >>= fromEither
  --or use a fx designed for it:
  name <- createSafeNameWithError nameAsText
  makeEmail name

--
runCreateNameAndEmailInIO :: IO ()
runCreateNameAndEmailInIO = do
  print $ createNameAndEmail "Heathy" & interpretName & interpretEmail & runError & run

runCreateNameAndEmail :: Either T.Text Email
runCreateNameAndEmail = 
  createNameAndEmail "Heath" & interpretName & interpretEmail & runError & run
------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------- from the examples in Polysemy.Internal
example :: Members '[State String, Error String] r => Sem r String
example = do
    put "start"
    let throwing, catching :: Members '[State String, Error String] r => Sem r String
        throwing = do
          modify (++"-throw")
          throw "error"
          get
        catching = do
          modify (++"-catch")
          get
    --catch @String throwing (\ _ -> catching)
    catch @String throwing (const catching)

runExampleHandleErrorFirst  =
  example
    & runError
    & fmap (either id id)
    & evalState ""
    & runM
    & (print =<<)
--outputs: "start-throw-catch"

runExampleHandleStateFirst =
  example
    & evalState ""
    & runError
    & fmap (either id id)
    & runM
    & (print =<<)
--outputs: "start-catch"