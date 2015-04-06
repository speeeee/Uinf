module Core.Nations (Region(..), Culture(..), nations) where

import Graphics.Rendering.OpenGL.Raw

data Region = Region { name :: String,
                       distro :: [(Double,Culture)],
                       poprnk :: Int,
                       rep :: Double,
                       friends :: [String],
                       enemies :: [String] } deriving (Show,Eq)

data Culture = Culture { cname :: String,
                         crep :: Double,
                         cfriends :: [String],
                         cenemies :: [String],
                         color :: (GLfloat,GLfloat,GLfloat) } deriving (Show,Eq)

mkculture name (r,g,b) = Culture name 0 [] [] (r,g,b)

nations :: [Region]
nations =
  [(Region "central america" [(100,mkculture "central american" (1.0,0.3,0.3))] 22 0 [] []), --1
   (Region "andes" [(100,mkculture "andean" (0.8,0.8,0.1))] 20 0 [] []),                     --2
   (Region "south america" [(100,mkculture "south american" (0.3,1.0,0.3))] 8 0 [] []),      --3
   (Region "north america" [(100,mkculture "north american" (0.8,0.1,0.8))] 9 0 [] []),      --4
   (Region "west africa" [(100,mkculture "west african" (0.96,0.96,0.86))] 13 0 [] []),      --5
   (Region "north africa" [(100,mkculture "north african" (0.5,0.5,0.2))] 12 0 [] []),       --6
   (Region "central africa" [(100,mkculture "central african" (0.7,0.1,0.2))] 15 0 [] []),   --7
   (Region "east africa" [(100,mkculture "east african" (1.0,0.0,0.0))] 14 0 [] []),         --8
   (Region "south africa" [(100,mkculture "south african" (0.8,0.3,0.3))] 10 0 [] []),       --9
   (Region "western europe" [(100,mkculture "western european" (0.3,0.3,0.8))] 6 0 [] []),   --10
   (Region "eastern europe" [(100,mkculture "eastern european" (0.8,0.3,0.8))] 7 0 [] []),   --11
   (Region "scandinavia" [(100,mkculture "scandinavian" (0.8,0.8,1.0))] 21 0 [] []),         --12
   (Region "middle east" [(100,mkculture "middle eastern" (0.8,0.5,0.5))] 11 0 [] []),       --13
   (Region "caucasus" [(100,mkculture "caucasian" (0.9,0.2,0.0))] 24 0 [] []),               --14
   (Region "west eurasia" [(100,mkculture "west eurasian" (0.9,0.5,0.9))] 16 0 [] []),       --15
   (Region "siberia" [(100,mkculture "siberian" (0.8,1.0,0.8))] 25 0 [] []),                 --16
   (Region "eastern asia" [(100,mkculture "eastern asian" (0.7,0.7,0.7))] 2 0 [] []),        --17
   (Region "central asia" [(100,mkculture "central asian" (0.8,0.6,0.3))] 1 0 [] []),        --18
   (Region "oceania" [(100,mkculture "oceanian" (0.8,0.5,0.3))] 17 0 [] [])]                 --19
