import qualified Data.Map as Map

data Grade = F | D | C | B | A deriving (Show, Eq, Ord, Enum, Read)

data Degree = HS | BA | MS | PhD deriving (Show, Eq, Ord, Enum, Read)

data Candidate = Candidate
    { candidateId :: Int
    , codeReview :: Grade
    , cultureFit :: Grade
    , education :: Degree } deriving Show

isViable :: Candidate -> Bool
isViable cand = all (== True) tests
    where passedCodeTest = codeReview cand > B
          passedCultureFit = cultureFit cand > C
          educationMin = education cand >= MS
          tests = [passedCodeTest, passedCultureFit, educationMin]

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

getCandidateIO :: IO Candidate
getCandidateIO = pure Candidate <*> readInt <*> readGrade <*> readGrade <*> readDegree

candidate1 :: Candidate
candidate1 = Candidate 1 A C BA

candidate2 :: Candidate
candidate2 = Candidate 2 C B HS

candidate3 :: Candidate
candidate3 = Candidate 3 A B MS

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1, candidate1), (2, candidate2), (3, candidate3)]

getCandidateMaybe :: Int -> Maybe Candidate
getCandidateMaybe id = Map.lookup id candidateDB

checkCandidate :: Monad m => m Candidate -> m String
checkCandidate candidates = do
    candidate <- candidates
    let passed = isViable candidate
    let result = if passed
        then "Passed"
        else "Failed"
    return result