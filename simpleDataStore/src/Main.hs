type Name = String
type Surname = String
type Age = Int
type Record = (Name, Surname, Age)

-- function, that combines lists to list of cortages

combineDatabase :: [Name] -> [Surname] -> [Age] -> [Record]
combineDatabase name surname age = zip3 name surname age

-- function, that converts record to String

showRecord :: Record -> String
showRecord (name, surname, age) = name ++ " " ++ surname ++ " " ++ show(age)

-- now, create three lists, combine it to the list of rows, and then show it

main = 
       let names = ["Test", "Andre", "Ivan"]
           surnames = ["Test", "Kuzon", "Kester"]
           ages = [13, 16, 28]
           list = map showRecord $ combineDatabase names surnames ages
       in putStr (unlines list)