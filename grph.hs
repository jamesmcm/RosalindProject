import Rosalind

main = do
     l <- getLine
     let l' = dnastr2bases l
     let z = dna2rna l'
     let l'' = rnabases2str z
     putStrLn l''