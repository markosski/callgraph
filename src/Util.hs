module Util where

import Data.Text (Text)

calculateEdgeWeight :: Integer -> Float
calculateEdgeWeight x
  | logValue < minValue = minValue
  | logValue > maxValue = maxValue
  | otherwise = logValue
  where maxValue = 6.0 :: Float
        minValue = 1.0 :: Float
        logValue = log(fromInteger x :: Float)

-- rawLogs :: Text
-- rawLogs = "Main function to application cg_start(main)\n\
-- \This is something else.\n\
-- \This is another useless message \n\
-- \Call database cg_start(get_user)\n\
-- \cg_end(get_user)\n\
-- \## Sending email cg_start(send_email) ##\n\
-- \cg_end(send_email)\n\
-- \## something else ##\n\
-- \Call database cg_start(get_user)\n\
-- \cg_end(get_user)\n\
-- \Wrapping up\n\
-- \Existing application cg_end(main)\n"