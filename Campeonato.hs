import Data.Function
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.Regex.Posix
import HSH

data Estado = Avenceu | Bvenceu | Empate deriving (Show)

data Jogador = Jogador {execPlayerName :: String, path :: FilePath, playerID :: String, points :: Int}
    deriving (Show)

execPath :: Jogador -> FilePath
execPath p = combine (path p) (execPlayerName p)

type GameResult = (String,Estado)
type Score = (String,Int)

--Makes all pair of combinations of a list, without repetition
combination :: [a] -> [(a,a)]
combination [] = []
combination (x:xs) = map ((,)x) xs ++ combination xs

doMake :: FilePath -> IO ()
doMake path = do
    setCurrentDirectory path
    runIO "make"

getExecName :: FilePath -> IO String
getExecName path = do
    setCurrentDirectory path
    let playerFile = combine path "nome_do_jogador"
    fileExists <- doesFileExist playerFile
    if fileExists
        then readFile playerFile >>= (return . head . lines)
        else return "jogador"

getPlayerID :: FilePath -> String
getPlayerID fp =
    if file == "jogador"
        then folder 
        else file
    where dirs = splitDirectories fp
          file = last dirs
          folder = last (init dirs)

getLogfileName :: Int -> Jogador -> Jogador -> String
getLogfileName game p1 p2 =
    (playerID p1) ++ "vs" ++ (playerID p2) ++ "-" ++ (show game) ++ ".log"

getBoard :: IO String
getBoard = run "./genBoard"

twoGames :: Int -> String -> FilePath -> (Jogador,Jogador) -> IO [Score]
twoGames runID board test (p1, p2) = do
    score1 <- runGameWithId runID board test (p1,p2)
    score2 <- runGameWithId runID board test (p2,p1)
    return (score1 ++ score2)


getWinner :: String -> Either String Estado
getWinner log =
    if fim =~ "^Empate*" :: Bool
        then Right Empate
    else if fim =~ "^A *" :: Bool
        then Right Avenceu
        else if fim =~ "^B *" :: Bool
            then Right Bvenceu
            else Left "Erro ao determinar vitoria"
    where fim = last $ lines log


runGameWithId :: Int -> String -> FilePath -> (Jogador, Jogador) -> IO [Score]
runGameWithId runID board testDir (p1,p2) = do
    let logfileName = getLogfileName runID p1 p2
    putStrLn logfileName
    (log,st) <- runGame board (execPath p1) (execPath p2)
    writeFile (combine testDir logfileName) log
    case st of
        Avenceu -> return [((playerID p1),3)]
        Bvenceu -> return [((playerID p2),3)]
        Empate  -> return [(playerID p1,1),(playerID p2,1)]

runGame :: String -> FilePath -> FilePath -> IO GameResult
runGame board p1 p2 = do
    log <- run $ (echo board) -|- ("./MainProlog " ++ p1 ++ " " ++ p2)
    case getWinner log of
        Left msg -> error msg
        Right st -> return (log, st)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

getPlayer :: FilePath -> FilePath -> IO Jogador
getPlayer base fp = do
    execName <- getExecName fp
    let playerID = getPlayerID (combine fp execName)
    return (Jogador execName (makeRelative base fp) playerID 0)

samePlayer :: Score -> Score -> Bool
samePlayer s1 s2 = fst s1 == fst s2

joinScore :: [Score] -> Score
joinScore scores = (fst $ head scores,total)
    where total = sum $ map snd scores

score :: [Score] -> [Score]
score scores = map joinScore scoreGroup
    where scoreGroup = groupBy samePlayer (sort scores)

showScore :: Score -> String
showScore s = (fst s) ++ "\t" ++ (show (snd s))

showScoreList :: [Score] -> String
showScoreList xs = unlines (("ID do jogador\tPontuacao"):(map showScore xs))

campeonato :: FilePath -> [Jogador] -> Int -> IO [Score]
campeonato testDir players id = do
    board <- getBoard
    let combs = combination players
    scoreLists <- sequence $ map (twoGames id board testDir) combs
    let scores = reverse $ sortBy (compare `on` snd) $ score $ concat scoreLists
    writeFile (combine testDir ("campeonato-" ++ (show id) ++ ".rank")) (showScoreList scores)
    return scores

--Zip the player binary in a file
zipBin :: Jogador -> IO ()
zipBin j = runIO ("zip -j bins.zip " ++ exec)
    where exec = execPath j

main = do
    args <- getArgs
    baseDir <- getCurrentDirectory
    let testDir = combine baseDir (head args) 
    setCurrentDirectory testDir
    runIO "unzip \\*.zip"
    files <- getDirectoryContents testDir
    let folders = filter (=~ "^(r|R)(a|A)*") files
    print folders
    sequence $ map (doMake . (combine testDir)) folders --compile players
    players <- sequence $ map (getPlayer baseDir . (combine testDir)) folders
    sequence $ map zipBin players --zip binaries
    setCurrentDirectory baseDir
    rankings <- sequence $ map (campeonato testDir players) [1..5]
    let globalRank = showScoreList $ reverse $ sortBy (compare `on` snd) $ score $ concat rankings
    writeFile (combine testDir "total.rank") globalRank
    setCurrentDirectory testDir
    runIO "find . -name \"*.log\" -print | zip teste -@"
    runIO "find . -name \"*.rank\" -print | zip teste -@"
