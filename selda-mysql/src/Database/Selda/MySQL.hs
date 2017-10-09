{-# LANGUAGE OverloadedStrings, RecordWildCards, GADTs, CPP #-}
-- | MySQL backend for Selda.
module Database.Selda.MySQL
  ( ConnectInfo (..)
  , withMySQL, on, auth
  , mysqlBackend
  ) where
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import Database.Selda.Backend
import Database.MySQL.Base as MySQL
import Control.Monad.Catch

#ifndef __HASTE__
import Database.Selda.MySQL.Encoding
import qualified Data.ByteString.Char8 as BS
#endif

-- | Perform the given computation over a MySQL database.
--   The database connection is guaranteed to be closed when the computation
--   terminates.
withMySQL :: (MonadIO m, MonadThrow m, MonadMask m)
               => MySQL.ConnectInfo -> SeldaT m a -> m a
#ifdef __HASTE__
withMySQL _ _ = return $ error "withMySQL called in JS context"
#else
withMySQL ci m = do
  conn <- liftIO $ connect ci
  let backend = mysqlBackend (T.pack $ show ci) conn
  runSeldaT m backend `finally` liftIO (finish conn)

-- | Create a `SeldaBackend` for MySQL `Connection`
mysqlBackend :: MySQL.MySQLConn   -- ^ MySQL connection object.
          -> SeldaBackend
mysqlBackend c = SeldaBackend
  { runStmt         = \q ps -> right <$> mysqlQueryRunner c False q ps
  , runStmtWithPK   = \q ps -> left <$> mysqlQueryRunner c True q ps
  , prepareStmt     = mysqlPrepare c
  , runPrepared     = pgRun c
  , backendId       = MySQL
  , ppConfig        = defPPConfig
    { ppType = mysqlPPType
    , ppPlaceholder = \ _ -> "?"
    , ppAutoIncInsert = "NULL"
    , ppColAttrs = mysqlPPColAttrs
    }
  , closeConnection = \_ -> finish c
  }
  where
    left (Left x) = x
    left _        = error "impossible"
    right (Right x) = x
    right _         = error "impossible"

mysqlPrepare :: MySQL.MySQLConn -> StmtID -> [SqlTypeRep] -> T.Text -> IO Dynamic
mysqlPrepare c _ _ q = do
    mres <- MySQL.prepare c (MySQL.Query $ encodeUtf8 q)
    return (toDyn mres)

mysqlRun :: MySQL.MySQLConn -> Dynamic -> [Param] -> IO (Int, [[SqlValue]])
mysqlRun c hdl ps = do
    let Just sid = fromDynamic hdl :: Maybe StmtID
    mres <- execPrepared c (BS.pack $ show sid) (map mkParam ps) Text
    unlessError c errmsg mres $ getRows
  where
    errmsg = "error executing prepared statement"
    mkParam (Param p) = case fromSqlValue p of
      Just (_, val, fmt) -> Just (val, fmt)
      Nothing            -> Nothing

mysqlQueryRunner :: MySQL.MySQLConn -> Bool -> T.Text -> [Param] -> IO (Either Int (Int, [[SqlValue]]))
mysqlQueryRunner c return_lastid q ps = do
    mres <- execParams c (encodeUtf8 q') [fromSqlValue p | Param p <- ps] Text
    case mres of
      Just res -> do
        st <- resultStatus res
        case st of
          BadResponse       -> throwM $ SqlError "bad response"
          FatalError        -> throwM $ SqlError errmsg
          NonfatalError     -> throwM $ SqlError errmsg
          _ | return_lastid -> Left <$> getLastId res
            | otherwise     -> Right <$> getRows res
      Nothing           -> throwM $ DbError "unable to submit query to server"
  where
    errmsg = "error executing query `" ++ T.unpack q' ++ "'"
    q' | return_lastid = q <> " RETURNING LASTVAL();"
       | otherwise     = q

    getLastId res = (readInt . maybe "0" id) <$> getvalue res 0 0

    getRows res = do
      rows <- ntuples res
      cols <- nfields res
      types <- mapM (ftype res) [0..cols-1]
      affected <- cmdTuples res
      result <- mapM (getRow res types cols) [0..rows-1]
      pure $ case affected of
        Just "" -> (0, result)
        Just s  -> (readInt s, result)
        _       -> (0, result)

    getRow res types cols row = do
      sequence $ zipWith (getCol res row) [0..cols-1] types

    getCol res row col t = do
      mval <- getvalue res row col
      case mval of
        Just val -> pure $ toSqlValue t val
        _        -> pure SqlNull

-- | Custom column types for postgres: auto-incrementing primary keys need to
--   be @BIGSERIAL@, and ints need to be @INT8@.
mysqlPPColAttrs :: [ColAttr] -> T.Text
mysqlPPColAttrs = T.unwords . map mysqlColAttr
  where
    -- | MySQL use @AUTO_INCREMENT@ to present AutoIncrement
    mysqlColAttr :: ColAttr -> Text
    mysqlColAttr AutoIncrement = "AUTO_INCREMENT"
    mysqlColAttr x             = pgColType defPPConfig x

-- | Custom column types for MySQL.
mysqlPPType ::  SqlTypeRep -> T.Text
mysqlPPType TRowID    = "BIGINT"
mysqlPPType TInt      = "BIGINT"
mysqlPPType TDateTime = "TIMESTAMP"
mysqlPPType t         = ppType defPPConfig t

#endif
