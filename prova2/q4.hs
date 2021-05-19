converter::[Int]->[String ]
converter [] = []
converter (x:xs) = num : converter xs
    where num = show x