import qualified LockFree as LF

main :: IO ()
main = do
    list <- LF.newEmptyList
    LF.add list 10
    putStrLn . show $ LF.toPureList list
