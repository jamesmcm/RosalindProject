-- UUU F      CUU L      AUU I      GUU V
-- UUC F      CUC L      AUC I      GUC V
-- UUA L      CUA L      AUA I      GUA V
-- UUG L      CUG L      AUG M      GUG V
-- UCU S      CCU P      ACU T      GCU A
-- UCC S      CCC P      ACC T      GCC A
-- UCA S      CCA P      ACA T      GCA A
-- UCG S      CCG P      ACG T      GCG A
-- UAU Y      CAU H      AAU N      GAU D
-- UAC Y      CAC H      AAC N      GAC D
-- UAA Stop   CAA Q      AAA K      GAA E
-- UAG Stop   CAG Q      AAG K      GAG E
-- UGU C      CGU R      AGU S      GGU G
-- UGC C      CGC R      AGC S      GGC G
-- UGA Stop   CGA R      AGA R      GGA G
-- UGG W      CGG R      AGG R      GGG G 


main = do
     l <- getContents
     putStrLn $ (encode l)

encode :: [Char] -> [Char]
encode [] = []
encode ('U':'U':'U':end) = 'F' : encode end
encode ('U':'U':'C':end) = 'F' : encode end
encode ('U':'U':_:end) = 'L' : encode end
encode ('U':'C':_:end) = 'S' : encode end
encode ('U':'A':'U':end) = 'Y' : encode end
encode ('U':'A':'C':end) = 'Y' : encode end
encode ('U':'A':_:end) = []
encode ('U':'G':'U':end) = 'C' : encode end
encode ('U':'G':'C':end) = 'C' : encode end
encode ('U':'G':'A':end) = []
encode ('U':'G':'G':end) = 'W' : encode end
encode ('C':'U':_:end) = 'L' : encode end
encode ('C':'C':_:end) = 'P' : encode end
encode ('C':'A':'U':end) = 'H' : encode end
encode ('C':'A':'C':end) = 'H' : encode end
encode ('C':'A':_:end) = 'Q' : encode end
encode ('C':'G':_:end) = 'R' : encode end
encode ('A':'U':'G':end) = 'M' : encode end
encode ('A':'U':_:end) = 'I' : encode end
encode ('A':'C':_:end) = 'T' : encode end
encode ('A':'A':'U':end) = 'N' : encode end
encode ('A':'A':'C':end) = 'N' : encode end
encode ('A':'A':_:end) = 'K' : encode end
encode ('A':'G':'U':end) = 'S' : encode end
encode ('A':'G':'C':end) = 'S' : encode end
encode ('A':'G':_:end) = 'R' : encode end
encode ('G':'U':_:end) = 'V' : encode end
encode ('G':'C':_:end) = 'A' : encode end
encode ('G':'A':'U':end) = 'D' : encode end
encode ('G':'A':'C':end) = 'D' : encode end
encode ('G':'A':_:end) = 'E' : encode end
encode ('G':'G':_:end) = 'G' : encode end