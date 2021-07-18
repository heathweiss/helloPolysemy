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
{------------------ Test out Trace, using newtype Name.----------------------

Test out the trace function.
Note that did not create a Sem monad from the Name ADT. That will be done later.

Because Name is not a Sem monad:
Do not need to run an interpreter for it.
Can't write high level code using it, as there is not interpreter.
There are no multiple constructors for it.
-}


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

{------------------ create an instance of Sem for the Name newtype, so can use it with effects.

-}
data NameSem m a where
  --Make a Name, without wrapping it in Either.
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

--Smart constructor for Name, which uses Either to ensure the name is "Heath"
--Gets called by an interpreter.
newSafeName :: T.Text -> Either T.Text Name
newSafeName name = 
  if name == "Heath" then Right $ Name name
    else
      Left "name was not Heath"

--interpretName :: Member NameSem r => Sem (NameSem : r) a -> Sem r a 
--Do not put the  'Member NameSem r =>' in there, as it then has that effect, 
--which then has to be interpreted.
interpretName :: Sem (NameSem : r) a -> Sem r a 
interpretName = interpret $ \case
  MakeName nameString -> return $ Name nameString
  MakeSafeName nameString -> return $ newSafeName nameString

--Requires a Name to build an Email.
--The Name can be made directly, or with newSafeName, in which case the Error effect is required. 
interpretEmail :: Sem (EmailSem : r) a -> Sem r a
interpretEmail = interpret $ \case
  MakeEmail (Name name) -> return $ Email (name <> "@hotmail.com")

runCreateName :: IO ()
runCreateName = do
  --No need to use the createName fx, call makeName directly.
  print $ makeName "Heath" & interpretName & run
  return ()


----------------------------------------------------------------------------------------------------------------------------------------------------------
{-
Create a Name, without the Error effect for simplicity, and add Trace effect to it.
-}

--Combine the use of NameSem and Trace effects, to make a Name with tracing.
makeEffectfulNameWithTrace :: forall r. Members '[Trace,NameSem] r => Sem r Name
makeEffectfulNameWithTrace = do
  trace "make name: Heath"
  makeName "Heath"

--print the trace statements with IO
runCreateEffectfulNameAndTrace :: IO ()
runCreateEffectfulNameAndTrace = do
  let (s,a) = makeEffectfulNameWithTrace & interpretName & runTraceList & run
  mapM_ print s
  print a
  return ()

--print the trace statements using traceToIO interpretation.
runCreateEffectfulNameAndTraceWithoutIO :: IO Name
runCreateEffectfulNameAndTraceWithoutIO = 
  makeEffectfulNameWithTrace & interpretName & traceToIO  & runM
  
  ----------------------------------------------------------------------------------------------------------------------------------------------------------
{-
Create a safe name, handle with and without the Error effect.

Use the MakeSafeName constructor of NameSem, to get Either Text Name

Will be an error to create a name that is not Heath.
-}



--High level effects code
--All is does is call makeSafeName, so it is pointless, but would be req'd if it was more complex.
--Eg: createSafeNameWithError calls makeSafeName and binds it to fromEither to add the Error effect.
createSafeName :: Member NameSem r => T.Text -> Sem r (Either T.Text Name)
createSafeName nameAsText = do
  makeSafeName nameAsText

--Create a safe Name, and add the Error effect to it.
createSafeNameWithError :: Members '[NameSem, Error T.Text] r => T.Text -> Sem r Name
createSafeNameWithError nameAsText = do
  makeSafeName nameAsText >>= fromEither 

--Create the high level Effects code, which later get interpreted.
createNameAndEmail :: Members '[NameSem, EmailSem, Error T.Text] r => T.Text -> Sem r Email
createNameAndEmail nameAsText = do
  --create an Either Text Name, then remove the Either with the Error effect
  --nameE <- makeSafeName nameAsText
  --name <- fromEither nameE

  --or combine with bind operator in a single step
  --name <- makeSafeName nameAsText >>= fromEither

  --or use the createSafeNameWithError fx, which is a wrapper around above calls.
  name <- createSafeNameWithError nameAsText
  makeEmail name

--interpret the createNameAndEmail high level effects code, using the IO monad.
--Anything but "Heath" will throw error.
runCreateNameAndEmailInIO :: IO ()
runCreateNameAndEmailInIO = do
  print $ createNameAndEmail "Heathy" & interpretName & interpretEmail & runError & run

--interpret the createNameAndEmail high level effects code, without using IO monad.
--Anything but "Heath" will throw error.
runCreateNameAndEmail :: Either T.Text Email
runCreateNameAndEmail = 
  createNameAndEmail "Heath" & interpretName & interpretEmail & runError & run

-------------------------------------------------------------------------------------------------------------------------------------------------------
{-------------------------- Create Name with Error and Trace effects---------------------------------}
createNameWithErrorAndTrace :: Members '[NameSem, Error T.Text, Trace] r => T.Text -> Sem r Name
createNameWithErrorAndTrace nameAsText = do
  trace $ "input: " ++ show nameAsText
  makeSafeName nameAsText >>= fromEither 

runCreateNameWithErrorAndTrace :: IO ()
runCreateNameWithErrorAndTrace = do
  --note: it is the runM, that handles the Embed IO effect, that comes from traceToIO. using 'run' will not compile due to unhandled effect.
  t <- createNameWithErrorAndTrace "Heath" & interpretName & interpretEmail & runError & traceToIO & runM
  print t 
  return ()
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