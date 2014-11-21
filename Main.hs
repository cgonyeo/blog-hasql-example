{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings #-}

import qualified Hasql as H
import qualified Hasql.Postgres as H
import qualified Data.Int as I

psqlConf :: H.Postgres
psqlConf = H.Postgres "haruko.gonyeo.com" 5432 "psql-user" "s3kr1t" "my-database"

main :: IO ()
main = do s <- maybe (fail "Improper session settings") return $
                        H.sessionSettings 6 30
          H.session psqlConf s $ do
              initDb
              saveData 1.1
              saveData 1.1

initDb :: H.Session H.Postgres IO ()
initDb = H.tx Nothing $ do
              H.unit [H.q|DROP TABLE IF EXISTS data|]
              H.unit [H.q|CREATE TABLE data (
                              field1    DECIMAL NOT NULL,
                              field2    BIGINT  NOT NULL,
                              PRIMARY KEY (field1)
                          )|]

saveData :: Double -> H.Session H.Postgres IO ()
saveData num = 
            H.tx (Just (H.Serializable, True)) $ do
                (mrow :: Maybe (Double,I.Int64))
                    <- H.single $ [H.q|SELECT *
                                       FROM data
                                       WHERE field1=?|] num
                case mrow of
                    Just (field1,field2) ->
                                let newfield2 = field2 * 2 + 1
                                in H.unit $ [H.q|UPDATE data
                                                 SET field2=?
                                                 WHERE field1=?|] newfield2 field1
                    Nothing ->  H.unit $ [H.q|INSERT INTO data
                                                  ( field1
                                                  , field2
                                                  ) VALUES (?,?)|] num (1 :: I.Int64)
