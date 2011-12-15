{-# LANGUAGE QuasiQuotes #-}

-- | Paste has a Pony, Rack has a Lobster, Sack has a Lambda ><

module Sack.Contrib.Middleware.Lambda (lambda) where

import Data.Default
import Sack
import Sack.Contrib.Constants
import Sack.Contrib.Response
import Sack.Contrib.Utils
import Air
import Air.Heavy
import Air.Extra
import Air.TH
import Prelude hiding ((.), (^), (>), (+), (-))
import qualified SafeBase.ByteString.Char8 as B


data64 :: String
data64 = [$here|

H4sIAAAAAAAAA+3dMVMbMRQE4J5foaGhpMhMCkP4LcpknCrBN0BB/n1yIbHkRNKd
pPe0j7vdjsa6/cYbjAm+++np6J5ffnw7frr+enp8ObiP0+vth+nVfT89np6nz1+O
d9cPXiQOHJESVyKPgqaQwRCxQEPMEaghYYFmeEt/DwELNMLfGLBAE4TALdAAccAW
6PqXgVqgy/8TpAW6+3+hRRSYBbp4KiALdO1kMBbo1pnQIgrAAl05n+EW6MKF0CLK
YAt03XJoETLUAl12KQMt0FUXQ4sowyzQRVeEFlEGWaBrrgotogyxQJdcGVpEGWCB
rrg6tIiiboEuWBFaRKFFiLIFul5VaBGFFiGqFuhylaFFFFqEKFqgq1WHFlFoEaJm
gS7WEFpEoUUILUKULNqv5+acX1/dnSNWuHS2MQsXLOYHOVschPqWYs7CXTwxDkaf
GIMsgCsxZwFciYpF3yXBVmLQArYSgxawlVi0gK3EogVqJRYtLp8Y41Zi0gK0EpsW
hZXcJNN/pLNqUVrJu7IQua78SvzuLCpXInOmUYvKlYgcadaibiUiJ9q1GP+Ky67F
+Fdc78JifsgBTwy7FrmN6GHYtcj825n+XiJyolmL3ELS30tEjrRqkV9I8hWXzJlG
LfILSa5E5EwFCy9wWaWFpFYicKRVi/JCEivpP9JZtcgsRPkVl0mLzEIOyu9xWbQo
/SSi+YrLokV2IXMUV2LQIr+QOYorsWex9LO62krWF7Tzu2WtlahY+J4rKi9kjtZK
zFmseTdLaSXmLBYXMkdnJdYslhcyR2clxizWvt+rsZKKfvy/rrSgxb4taurxb2lo
QYtdW1S149+n0mJ/FnXl+DkHtJCw8Oh6NamsRot9WNRW42eN7cKiuhk/m5AWQhYe
XXJd6nvRos/Co2uuSUMtWnRaeHTR5bS02qhFUyveW2HrFm2leP+RfguPrltKY6Ut
WrRW4j29JCw8unIuzYW2Z9FeaHP3hgRZeHTtVDrqbM2ip87G7qvb1WZb956GWnh0
+cv0deG96gUtPBogpLfJhiy6m/RbeLTBn/QXEbDwaIXfEeghYeHRDk6EQsbCoyWc
SIur+9vp6fjwE8s/QZd0jAAA

|].strip

lambda :: Middleware
lambda app = \env -> do
  if env.path_info.is "/lambda"
    then 
      return - def
        .set_body (data64.unzip64.B.pack)
        .set_content_type _TextHtml
        .set_status 200
    
    else app env
