data Color = Red | Black | Green | Blue | Pink 
  deriving (Show, Eq)

type Name = String
type Age = Int

data Person = Person Name Age Color
  deriving Show

getName :: Person -> Name
getName (Person a _ _) = a

data PersonOrColor = CombinedPerson Person |
                      CombinedColor Color

ryan :: PersonOrColor
ryan = CombinedPerson(Person "IceColdEdge" 41 Black)

personNameOrColorName ::  PersonOrColor -> String 
personNameOrColorName (CombinedPerson x) = getName x
personNameOrColorName (CombinedColor x) = show x