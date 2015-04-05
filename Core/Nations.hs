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
  [(Region "central america" [(100,(Culture "central american" 0 [] []))] 22 0 [] []), --1
   (Region "andes" [(100,(Culture "andean" 0 [] []))] 20 0 [] []),                     --2
   (Region "south america" [(100,(Culture "south american" 0 [] []))] 8 0 [] []),      --3
   (Region "north america" [(100,(Culture "north american" 0 [] []))] 9 0 [] []),      --4
   (Region "west africa" [(100,(Culture "west african" 0 [] []))] 13 0 [] []),         --5
   (Region "north africa" [(100,(Culture "north african" 0 [] []))] 12 0 [] []),       --6
   (Region "central africa" [(100,(Culture "central african" 0 [] []))] 15 0 [] []),   --7
   (Region "east africa" [(100,(Culture "east african" 0 [] []))] 14 0 [] []),         --8
   (Region "south africa" [(100,(Culture "south african" 0 [] []))] 10 0 [] []),       --9
   (Region "western europe" [(100,(Culture "western european" 0 [] []))] 6 0 [] []),   --10
   (Region "eastern europe" [(100,(Culture "eastern european" 0 [] []))] 7 0 [] []),   --11
   (Region "scandinavia" [(100,(Culture "scandinavian" 0 [] []))] 21 0 [] []),         --12
   (Region "middle east" [(100,(Culture "middle eastern" 0 [] []))] 11 0 [] []),       --13
   (Region "caucasus" [(100,(Culture "caucasian" 0 [] []))] 24 0 [] []),               --14
   (Region "west eurasia" [(100,(Culture "west eurasian" 0 [] []))] 16 0 [] []),       --15
   (Region "siberia" [(100,(Culture "siberian" 0 [] []))] 25 0 [] []),                 --16
   (Region "eastern asia" [(100,(Culture "eastern asian" 0 [] []))] 2 0 [] []),        --17
   (Region "central asia" [(100,(Culture "central asian" 0 [] []))] 1 0 [] []),        --18
   (Region "oceania" [(100,(Culture "oceanian" 0 [] []))] 17 0 [] [])]                 --19
