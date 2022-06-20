module Menu where
import UserModel

registrasiMenu :: IO ()
registrasiMenu = do
    putStrLn "masukkan username : "
    username <- getLine
    putStrLn "masukkan password : "
    passwd <- getLine
    inputDataUser username passwd
    putStrLn "data terinput, silahkan login"
    loginMenu

loginMenu :: IO ()
loginMenu = do 
            putStrLn "username : "
            username <- getLine
            putStrLn "password : "
            passwd <- getLine
            putStrLn "checking username .."
            -- to do : validasi login
            putStrLn "authenticated! \n"
            mainMenu

mainMenu :: IO ()
mainMenu = do 
    putStrLn "(i) Input Data Barang"
    putStrLn "(s) Show Data"
    putStrLn "(u) Update Data"
    putStrLn "(d) Delete Data"
    putStrLn "(q) Quit"
    pilih <- getLine
    case pilih of
        "i" -> do
            putStrLn "Input Data Barang"
        "s" -> do
            putStrLn "Show Data Barang"
        "u" -> do
            putStrLn "Update Data Barang"
        "d" -> do
            putStrLn "Delete Data Barang"
        "q" -> do
            putStrLn "bye! \n\n"
        _ -> do
            putStrLn "pilihan tidak tersedia \n\n"
            mainMenu