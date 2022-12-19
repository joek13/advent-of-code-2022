module Main where
import Data.Map (Map, empty, insert, (!), keys)

type Filename = [String]
data Entry = Dir [Filename] | File Int
    deriving Show
type FileTree = Map Filename Entry

isDir :: Entry -> Bool
isDir (Dir _) = True
isDir _ = False

-- resolves relative path to absolute path
getAbsolutePath :: Filename -> String -> Filename
-- "/" always resolves to root dir
getAbsolutePath _ "/" = []
-- ".." resolves to parent dir if it exists
getAbsolutePath (_:xs) ".." = xs
-- if working directory stack is empty we cannot navigate to ..
getAbsolutePath [] ".." = error "/ has no parent directory"
getAbsolutePath workingDir newDir = newDir:workingDir

parseLs :: Filename -> String -> (Filename, Entry)
parseLs workingDir input =
    let args = words input
        filename = getAbsolutePath workingDir (args !! 1) in
    case head args of
        "dir" -> (filename, Dir [])
        fSize -> (filename, File (read fSize))

readCmd :: Filename -> FileTree -> [String] -> (Filename, FileTree)
readCmd workingDir fileTree batch =
    let cmd = head batch
        output = tail batch in
    case words cmd of
        ["$", "cd", dir] -> (getAbsolutePath workingDir dir, fileTree)
        ["$", "ls"] -> 
            let files = map (parseLs workingDir) output
                filenames = map fst files
                newDir = Dir filenames
                fileTree' = insert workingDir newDir fileTree
                fileTree'' = foldl (\m (k, v) -> insert k v m) fileTree' files
            in 
                (workingDir, fileTree'')
        input -> error $ "unexpected command: " ++ unwords input

size :: FileTree -> Filename -> Int
size tree name =
    let entry = tree ! name in
    case entry of
        File fSize -> fSize
        Dir children -> sum $ map (size tree) children

main :: IO ()
main = do
    input <- getContents
    let inputLines = lines input
    let inputBatches = drop 1 $ foldr batch [] inputLines
            where batch nextLine (curBatch:batches)
                    | '$' `elem` nextLine = []:(nextLine:curBatch):batches
                    | otherwise = (nextLine:curBatch):batches
                  batch nextLine [] = [[nextLine]]
    let (_, fileTree) = foldl (uncurry readCmd) ([], empty) inputBatches
    print $ (sum . 
             -- filter
             filter (<=100000) . 
             -- compute size
             map (size fileTree) . 
             -- filter to directory filenames
             filter (\n -> isDir (fileTree ! n)) . 
             -- get keys (filenames)
             keys) fileTree