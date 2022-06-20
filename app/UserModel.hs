module UserModel where
import System.IO
import Data.List.Split

inputDataUser :: String -> String -> IO ()
inputDataUser username passwd = do
    let pathFile = "data/users.dat"
    let record = username ++ ";" ++ passwd ++ "\n"
    appendFile pathFile record

showDataUser :: IO ()
showDataUser = do
    fileHandle <- openFile "data/users.dat" ReadMode
    readData fileHandle

readData :: Handle -> IO ()
readData fileHandle = do
    isFileEnd <- hIsEOF fileHandle
    if isFileEnd 
            then
                putStrLn ""
            else
                do
                    record <- hGetLine fileHandle
                    putStrLn record
                    readData fileHandle
