module Main where
import Menu
import UserModel

main :: IO ()
main = do 
    putStrLn "(l) Login"
    putStrLn "(r) Registration"
    putStrLn "(q) Quit"
    pilih <- getLine
    case pilih of
        "l" -> loginMenu
        "r" -> do
            putStrLn "go to registration ..."
            registrasiMenu
        "q" -> do
            putStrLn "bye! \n\n"
        "t" -> showDataUser
        _ -> do
            putStrLn "pilihan tidak tersedia, pilih (l) untuk Login atau (r) untuk registrasi \n\n"
            main
