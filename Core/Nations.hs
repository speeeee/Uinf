module Core.Nations (Region(..), Culture(..), nations) where

data Region = Region { name :: String,
                       distro :: [(Double,Culture)],
                       poprnk :: Int,
                       rep :: Double,
                       friends :: [String],
                       enemies :: [String] } deriving (Show,Eq)

data Culture = Culture { cname :: String,
                         crep :: Double,
                         cfriends :: [String],
                         cenemies :: [String] } deriving (Show,Eq)

nations :: [Region]
nations =
  [(Region "eastern states" [(100,(Culture "eastern american" 0 [] []))] 4 0 [] []),
   (Region "western states" [(100,(Culture "western american" 0 [] []))] 5 0 [] []),
   (Region "central america" [(100,(Culture "central american" 0 [] []))] 22 0 [] []),
   (Region "andes" [(100,(Culture "andean" 0 [] []))] 20 0 [] []),
   (Region "south america" [(100,(Culture "south american" 0 [] []))] 8 0 [] []),
   (Region "caribbean" [(100,(Culture "caribbean" 0 [] []))] 23 0 [] []),
   (Region "north america" [(100,(Culture "north american" 0 [] []))] 9 0 [] []),
   (Region "west africa" [(100,(Culture "west african" 0 [] []))] 13 0 [] []),
   (Region "north africa" [(100,(Culture "north african" 0 [] []))] 12 0 [] []),
   (Region "central africa" [(100,(Culture "central african" 0 [] []))] 15 0 [] []),
   (Region "east africa" [(100,(Culture "east african" 0 [] []))] 14 0 [] []),
   (Region "south africa" [(100,(Culture "south african" 0 [] []))] 10 0 [] []),
   (Region "western europe" [(100,(Culture "western european" 0 [] []))] 6 0 [] []),
   (Region "eastern europe" [(100,(Culture "eastern european" 0 [] []))] 7 0 [] []),
   (Region "scandinavia" [(100,(Culture "scandinavian" 0 [] []))] 21 0 [] []),
   (Region "middle east" [(100,(Culture "middle eastern" 0 [] []))] 11 0 [] []),
   (Region "caucasus" [(100,(Culture "caucasian" 0 [] []))] 24 0 [] []),
   (Region "ural mountains" [(100,(Culture "uralic" 0 [] []))] 16 0 [] []),
   (Region "siberia" [(100,(Culture "siberian" 0 [] []))] 25 0 [] []),
   (Region "tibet" [(100,(Culture "tibetan" 0 [] []))] 26 0 [] []),
   (Region "north china" [(100,(Culture "northern chinese" 0 [] []))] 2 0 [] []),
   (Region "south china" [(100,(Culture "southern chinese" 0 [] []))] 3 0 [] []),
   (Region "india" [(100,(Culture "indian" 0 [] []))] 1 0 [] []),
   (Region "indochina" [(100,(Culture "indochinese" 0 [] []))] 19 0 [] []),
   (Region "indonesia" [(100,(Culture "indonesian" 0 [] []))] 18 0 [] []),
   (Region "oceania" [(100,(Culture "oceanian" 0 [] []))] 17 0 [] []),
   (Region "mongolia" [(100,(Culture "mongolian" 0 [] []))] 27 0 [] [])]
