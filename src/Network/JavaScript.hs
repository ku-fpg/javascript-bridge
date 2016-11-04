module Network.JavaScript where
	
import Data.Monoid
import qualified Data.Text.Lazy as LT
import qualified Network.Wai.Handler.WebSockets as WS
import Network.Wai (Application)
import Network.WebSockets as WS

start :: (Engine -> IO ()) -> Application -> Application
start k = WS.websocketsOr WS.defaultConnectionOptions $ \ pc -> do
  conn <- WS.acceptRequest pc
  k $ Engine conn

newtype Command = Command LT.Text

instance Monoid Command where
  mempty                        = Command mempty
  Command a `mappend` Command b = Command (a `mappend` b)

sendCommand :: Engine -> Command -> IO ()
sendCommand (Engine conn) (Command txt) = WS.sendTextData conn txt
	
{-
newtype Procedure a = Procedure LT.Text [LT.Text] (Value -> a)

zipProcedure :: Procedure a -> Procedure b -> Procedure (a,b)
zipProcedure 

sendProcedure :: Engine -> Procedure a -> IO a
sendProcedure (Engine conn) (Procedure txt) = WS.sendTextData conn txt
-}
----------------------------------------

data Engine = Engine WS.Connection

	