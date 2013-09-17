{-# LANGUAGE
    EmptyDataDecls
  , FlexibleContexts
  , GADTs
  , OverloadedStrings
  , QuasiQuotes
  , TemplateHaskell
  , TypeFamilies
  #-}

import Data.Maybe
import Data.UUID (UUID)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import qualified Data.UUID as UUID
import UUID ()
import Control.Monad.Error

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Foo
  bar UUID
  FooU bar
  deriving Show
|]

main :: IO ()
main = withPostgresqlPool connStr 10 $ \pool -> do
  flip runSqlPersistMPool pool $ do
--    runMigration migrateAll
    let u = fromJust $ UUID.fromString "497B3086-0AE2-4433-BF55-EDF3F5F78399"
    insert $ Foo u
    res <- getBy $ FooU u
    liftIO $ print res

connStr :: ConnectionString
connStr = "host=localhost dbname=persistent_test user=persistent password=persistent port=5432"
