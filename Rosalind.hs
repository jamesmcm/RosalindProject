module Rosalind
(Base,
dnabases2str,
dnastr2bases,
rnabases2str,
rnastr2bases,
countBases,
dna2rna,
rna2dna,
DNA,
RNA) where

import Data.Char


data Base a = A | C | G | N a deriving (Show)
data RNA = U deriving (Show)
data DNA = T deriving (Show)

-- Reading DNA functions
getDNABase :: Char -> Base DNA
getDNABase 'A' = A
getDNABase 'C' = C
getDNABase 'G' = G
getDNABase 'T' = (N T)
getDNABase _ = error "Not a valid DNA base"

dnabase2chr :: Base DNA -> Char
dnabase2chr A = 'A'
dnabase2chr C = 'C'
dnabase2chr G = 'G'
dnabase2chr (N T) = 'T'

dnastr2bases :: [Char] -> [Base DNA]
dnastr2bases x = map getDNABase x

dnabases2str :: [Base DNA] -> [Char]
dnabases2str x = map dnabase2chr x

-- Reading RNA functions
getRNABase :: Char -> Base RNA
getRNABase 'A' = A
getRNABase 'C' = C
getRNABase 'G' = G
getRNABase 'U' = (N U)
getRNABase _ = error "Not a valid RNA base"

rnabase2chr :: Base RNA -> Char
rnabase2chr A = 'A'
rnabase2chr C = 'C'
rnabase2chr G = 'G'
rnabase2chr (N U) = 'U'

rnastr2bases :: [Char] -> [Base RNA]
rnastr2bases x = map getRNABase x

rnabases2str :: [Base RNA] -> [Char]
rnabases2str x = map rnabase2chr x

-- First Challenge - counting bases. Output order (A, C, G, T/U)
countBases :: [Base a] -> (Integer, Integer, Integer, Integer)
countBases x = foldr incrementBase (0, 0, 0, 0) x

incrementBase :: (Base a) -> (Integer, Integer, Integer, Integer) -> (Integer, Integer, Integer, Integer)
incrementBase A (a,c,g,n) = (a+1,c,g,n)
incrementBase C (a,c,g,n) = (a,c+1,g,n)
              incrementBase G (a,c,g,n) = (a,c,g+1,n)
incrementBase (N _) (a,c,g,n) = (a,c,g,n+1)

--- Second Challenge - converting RNA to DNA, DNA to RNA
dna2rna :: [Base DNA] -> [Base RNA]
dna2rna x = map dnabase2rnabase x

dnabase2rnabase :: Base DNA -> Base RNA
dnabase2rnabase (N T) = (N U)
dnabase2rnabase A = A
dnabase2rnabase C = C
dnabase2rnabase G = G

rna2dna :: [Base RNA] -> [Base DNA]
rna2dna x = map rnabase2dnabase x

rnabase2dnabase :: Base RNA -> Base DNA
rnabase2dnabase (N U) = (N T)
rnabase2dnabase A = A
rnabase2dnabase C = C
rnabase2dnabase G = G
