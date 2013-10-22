module ThMagic where

import Language.Haskell.TH
import Data.Aeson
import GHC.Unicode

-- list of data constructor names
-- match expression given json string
th_toJSON_case_expr :: [String] -> (String -> MatchQ) -> ExpQ
th_toJSON_case_expr strNames matchGen =
    caseE (varE $ mkName "method") matchExprs
    where matchExprs = map matchGen strNames

th_toJSON_setter_request names =
    th_toJSON_case_expr names $ \name ->
            match (conP (mkName name) [newName "x" >>= varP])
                  (normalB [| $(stringE . translateSet $ name) .= (toJSON $(varE $ mkName "x")) |])
                  []

th_toJSON_getter_request names =
    th_toJSON_case_expr names $ \name ->
            match (conP (mkName name) [newName "_" >>= varP])
                  (normalB [| toJSON ($(stringE . translateGet $ name) :: String) |])
                  []

th_toJSON_action_request names =
    th_toJSON_case_expr names $ \name ->
            match (conP (mkName name) [])
                  (normalB [| toJSON ($(stringE . translateAction $ name) :: String) |])
                  []

getDataConstrNames :: Info -> [String]
getDataConstrNames (TyConI (DataD _ _ _ constructors _)) =
    flip map constructors $ \constr -> case constr of
        NormalC name _    -> nameBase name

translateAction :: String -> String
translateAction = camelCaseToLispStyle

translateGet :: String -> String
translateGet = camelCaseToLispStyle . drop 4

translateSet :: String -> String
translateSet = camelCaseToLispStyle . drop 4


-- usage:
-- $(reify ''ActionMethod >>= (translateDataCons translateAction))
translateDataCons :: (String -> String) -> Info -> [String]
translateDataCons translate tyConstr =
    map translate (getDataConstrNames tyConstr)

-- convert CamelCaseString to camel-case-string
camelCaseToLispStyle :: String -> String
camelCaseToLispStyle "" = ""
camelCaseToLispStyle (c:cs) = toLower c : (concat $ map f cs)
    where f char = if isUpper char
                   then '-' : toLower char : []
                   else char : []

