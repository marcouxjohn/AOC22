import Data.List (delete,minimum)

get_max::Integer -> [Integer] -> IO ()

get_max curr_sum maxes = do 
    line <- getLine 
    let x = (show line:: String)
    if x == "\"xyz\"" 
        then do 
            let m_new = maxes ++ [curr_sum]
            get_max 0 (delete (minimum m_new) m_new)
        else 
            if x == "\"a\"" 
                then do  
                    print(sum maxes)
                else do 
                    let a = init (drop 1 x)
                    let y = (read a::Integer)
                    get_max (curr_sum + y) maxes

main = do
    get_max 0 [0,0,0]
