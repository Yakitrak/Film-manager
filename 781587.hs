import Data.List
import Data.List.Split
import Data.Char
import System.Exit

-- MATHFUN --
-- UP781587 --

-- Types--------------------------------------------------------------------------------------------------------------------

data Film = Film { fName :: String
                 , fDirector :: String
                 , fYear :: Int
                 , fFans :: [String]
                 }
                 deriving (Eq,Ord,Show,Read)

testDatabase :: [Film]
testDatabase = [
    Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Sam", "Olga", "Tim"],    
    Film "The Fly" "David Cronenberg" 1986 ["Garry", "Dave", "Zoe", "Kevin", "Emma"],
    Film "Body Of Lies" "Ridley Scott" 2008 ["Bill", "Olga", "Tim", "Zoe", "Paula"],
    Film "Avatar" "James Cameron" 2009 ["Dave", "Amy", "Liz"],
    Film "Titanic" "James Cameron" 1997 ["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"],
    Film "The Departed" "Martin Scorsese" 2006 ["Wally", "Liz", "Kevin", "Tim", "Emma"],
    Film "Aliens" "Ridley Scott" 1986 ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"],
    Film "Kingdom Of Heaven" "Ridley Scott" 2005 ["Jo", "Wally", "Emma"],
    Film "Prometheus" "Ridley Scott" 2012 ["Kevin", "Tim", "Emma", "Jo", "Liz"],
    Film "E.T. The Extra-Terrestrial" "Steven Spielberg" 1982 ["Dave", "Amy", "Garry", "Ian", "Neal"],
    Film "Bridge of Spies" "Steven Spielberg" 2015 ["Wally", "Sam", "Dave", "Neal"],
    Film "Jaws" "Steven Spielberg" 1975 ["Dave", "Jo", "Zoe", "Wally", "Emma", "Kate"],
    Film "The Martian" "Ridley Scott" 2015 ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"],
    Film "The BFG" "Steven Spielberg" 2016 ["Sam", "Wally", "Dave", "Jo", "Kate"],
    Film "The Shawshank Redemption" "Frank Darabont" 1994 ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe"],
    Film "Gladiator" "Ridley Scott" 2000 ["Olga", "Neal", "Kate", "Heidi", "Bill", "Sam", "Zoe"],
    Film "The Green Mile" "Frank Darabont" 1999 ["Kevin", "Tim", "Emma", "Heidi"],
    Film "True Lies" "James Cameron" 1994 ["Sam", "Dave"],
    Film "Super 8" "J J Abrams" 2011 ["Kevin", "Tim", "Emma", "Olga", "Heidi"],
    Film "Minority Report" "Steven Spielberg" 2002 ["Kevin", "Kate", "Tim", "Emma", "Olga", "Jenny", "Zoe"],
    Film "War Horse" "Steven Spielberg" 2011 ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"],
    Film "Silence" "Martin Scorsese" 2016 ["Wally", "Emma", "Tim", "Heidi", "Bill", "Olga", "Jo"],
    Film "The Terminal" "Steven Spielberg" 2004 ["Kate", "Dave", "Jo", "Wally", "Emma"],
    Film "Star Wars: The Force Awakens" "J J Abrams" 2015 ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz", "Jo"],
    Film "Hugo" "Martin Scorsese" 2011 ["Wally", "Sam"]
    ]


-- Functional Code ---------------------------------------------------------------------------------------------------------

--1 - Add film
addFilm :: String -> String -> Int -> [Film] -> [Film]
addFilm n d y filmDB = Film n d y [] : filmDB

--2 - Display formatted database
dbAsString :: [Film] -> String
dbAsString filmDB =  unlines (map filmAsString filmDB)

--2helper - Single film as String
filmAsString :: Film -> String
filmAsString film = "Name: " ++ fName film ++ " | Director: " ++ fDirector film ++ " | Year: "
                       ++ show (fYear film) ++ " | Fans: " ++ show (length (fFans film))
                                              
--3 - All films after specific year
filmsAfterYear :: Int -> [Film] -> String 
filmsAfterYear year filmDB = dbAsString ([film | film <- filmDB, (fYear film) > year])

--4 - All films of a specific fan 
filmsFan :: String -> [Film] -> String
filmsFan fan filmDB = dbAsString ([film |   film <- filmDB, elem fan (fFans film)])

--5 - All fans of specfic film
fansOfFilm :: String -> [Film] -> String
fansOfFilm name filmDB = unlines (concat ([ (fFans film) | 
                        film <- filmDB, name == (fName film)]))
    
--6 - Add fan to database
addFan :: String -> String -> [Film] -> [Film]
addFan name fan filmDB = map (\(film) -> if ((fName film) == name) 
                                         then (Film (fName film) (fDirector film) (fYear film) (nub (fan : (fFans film))))
                                         else film) filmDB
                         
--7 - All fans of specific director
fanOfDirector :: String -> [Film] -> String
fanOfDirector director filmDB = unlines (nub (concat [ (fFans film) | film <- filmDB, director == (fDirector film) ]))

--8 - All directors, specific fan and number of films he likes
listDirectors :: String -> [Film] -> String
listDirectors fan filmDB = unlines (map (\(dir) -> dir ++ " : "  ++ show (length[ film 
                         | film <- filmDB, dir == (fDirector film) && elem fan ((fFans film))])) (getDirectors filmDB))

--8helper - List of all directors  
getDirectors :: [Film] -> [String]
getDirectors filmDB = nub [(fDirector film) | film <- filmDB]

-- User Interface Code -----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
       contents <- readFile "filmsFormatted.txt"
       let films = (read contents :: [Film] )
       putStrLn (dbAsString films)
       
       putStrLn "Welcome to UP781587 film database!"
       putStr "Enter your name: "
       name <- getLine
       putStrLn ("Hello, " ++ name)
       menu name films
    
--main interface of program - allows users to run all functions            
menu :: String -> [Film] -> IO ()
menu name films = do
    putStrLn ""
    putStrLn " ________________________________________ "
    putStrLn "| 1 | Show all films                     |"
    putStrLn "| 2 | Add new film                       |"
    putStrLn "| 3 | Show films after a specific year   |"
    putStrLn "| 4 | Show films you like                |"
    putStrLn "| 5 | Show fans of specific film         |"
    putStrLn "| 6 | Add yourself as a fan to film      |"
    putStrLn "| 7 | Show fans of specific director     |"
    putStrLn "| 8 | Show all directors you like        |"
    putStrLn "| 9 | Save and exit                      |"
    putStrLn "|___|____________________________________|"
    putStrLn ""
    putStr   "Please select an option 1 to 9: "

    option <- getLine
    case option of
        "1" -> do 
                putStrLn (dbAsString films)
                menu name films
                
        "2" -> do
               putStr "Enter the name of film: "
               filmName <- getLine
               putStr "Enter the director of film: "
               dir <-  getLine 
               putStr "Enter the year: "
               yr <- validateYear
               let newFilms = addFilm filmName dir yr films
               putStrLn "New film added!"
               menu name newFilms
                    
        "3" -> do
               putStr "Enter the year: "
               yr <- validateYear
               putStrLn ("Films afters " ++ show yr ++ ":")
               putStr (filmsAfterYear yr films)
               menu name films

        "4" -> do
               putStrLn "All films you like: "
               putStr (filmsFan name films)
               menu name films
        
        "5" -> do
               putStr "Enter the name of film: "
               filmName <- getLine
               putStrLn ("Fans of " ++ filmName ++ ":")                
               putStr (fansOfFilm filmName films)
               menu name films
        
        "6" -> do
               putStr "Enter the film name: "
               filmName <- getLine 
               let newFilms = addFan filmName name films
               putStrLn ("You are now a fan of " ++ filmName)
               menu name newFilms
               
        "7" -> do
               putStr "Enter the name of director: "
               dName <- getLine
               putStrLn ("Fans of " ++ dName ++ ":")
               putStr (fanOfDirector dName films)
               menu name films
               
        "8" -> do
               putStrLn "Directors you like:"
               putStr (listDirectors name films)
               menu name films
        
        "9" -> do
               putStr "You are about to save changes. Type [YES] to save changes: "
               str <- getLine
               let confirm = map toUpper str
               if (confirm /= "YES") then do
                                               putStrLn "File NOT updated!"
                                               return () 
               else do
               putStrLn "File [filmsFormatted.txt] updated. Thank you for using UP781587 film database!" 
               writeFile "filmsFormatted.txt" (show films)
               return ()
       
        _ -> do
             putStrLn "Please enter numbers 1 to 9"
             menu name films
        
    

    
--created to get validate year until its a number and 4 digits long   
validateYear :: IO Int
validateYear = do
                str <- getLine
                if checkAllDigits str == True && (length str == 4) then 
                    return (read str :: Int)
                else do
                    putStr "Enter the year (4 digit number): "
                    str <- validateYear
                    return (str)


checkAllDigits :: String -> Bool 
checkAllDigits xs = all isDigit xs 

    
-- Demo --------------------------------------------------------------------------------------------------------------------

demo :: Int -> IO ()
--demo 1  = putStrLn all films after adding 2017 film "Alien: Covenant" by "Ridley Scott" to testDatabase
demo 1 = putStrLn (dbAsString (addFilm "Alien: Covenant" "Ridley Scott" 2017 testDatabase))

--demo 2  = putStrLn (filmsAsString testDatabase)
demo 2 = putStrLn (dbAsString testDatabase)

--demo 3  = putStrLn all films released after 2008
demo 3 = putStrLn (filmsAfterYear 2008 testDatabase)

--demo 4  = putStrLn all films that "Liz" is a fan of
demo 4 = putStrLn (filmsFan "Liz" testDatabase)

--demo 5  = putStrLn all fans of "Jaws"
demo 5 = putStrLn (fansOfFilm "Jaws" testDatabase)

--demo 6  = putStrLn all films after "Liz" says she becomes fan of "The Fly"
demo 6 = putStrLn (dbAsString (addFan "The Fly" "Liz" testDatabase))

--demo 66 = putStrLn all films after "Liz" says she becomes fan of "Avatar"
demo 66 = putStrLn (dbAsString(addFan "Avatar" "Liz" testDatabase))

--demo 7 =  putStrLn all fans of films directed by "James Cameron"
demo 7 = putStrLn (fanOfDirector "James Cameron" testDatabase)

--demo 8  = putStrLn all directors & no. of their films that "Liz" is a fan of
demo 8 = putStrLn (listDirectors "Liz" testDatabase)