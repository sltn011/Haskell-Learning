import Control.Monad
import Control.Applicative

data Name = Name
    { firstName :: String
    , secondName :: String
    }

instance Show Name where
    show (Name first second) = mconcat [first, " ", second]

data GradeLevel = Freshman
                | Sophomore
                | Junior
                | Senior deriving (Eq, Ord, Enum, Show)

data Student = Student
    { studentId :: Int
    , gradeLevel :: GradeLevel
    , studentName :: Name
    } deriving Show

students :: [Student]
students = [ Student 1 Senior    (Name "Odri" "Lord")
           , Student 2 Junior    (Name "Lesly" "Silko")
           , Student 3 Freshman  (Name "Judith" "Bathler")
           , Student 4 Senior    (Name "Gi" "Debor")
           , Student 5 Sophomore (Name "Jan" "Bordijar")
           , Student 6 Junior    (Name "Jilia" "Christeva")
           ]

data Teacher = Teacher
    { teacherId :: Int
    , teacherName :: Name
    } deriving Show

teachers :: [Teacher]
teachers = [ Teacher 100 (Name "Simona" "de Bovuar")
           , Teacher 200 (Name "Suzen" "Zontag")
           ]

data Course = Course
    { courseId :: Int
    , courseTitle :: String
    , teacher :: Int } deriving Show

courses :: [Course]
courses = [ Course 101 "French language" 100
          , Course 201 "English language" 200
          ]

_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = do
    val <- vals
    return (prop val)

_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where test vals = do
    val <- vals
    guard (test val)
    return val

_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)
_join data1 data2 prop1 prop2 = do
    d1 <- data1
    d2 <- data2
    let dpair = (d1, d2)
    guard ((prop1 (fst dpair)) == (prop2 (snd dpair)))
    return dpair

_hinq selectQuery joinQuery whereQuery =
    (\joinData ->
        (\whereResult ->
            selectQuery whereResult
        ) (whereQuery joinData)
    ) joinQuery

exampleQuery1 :: [Name]
exampleQuery1 =
    _hinq (_select (teacherName . fst))
          (_join teachers courses teacherId teacher)
          (_where ((== "English language") . courseTitle . snd))

exampleQuery2 :: [String]
exampleQuery2 =
    _hinq (_select secondName)
          (exampleQuery1)
          (_where (\_ -> True))

data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a) | HINQ_ (m a -> m b) (m a)

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) =
    _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) =
    _hinq sClause jClause (_where (\_ -> True))

query1 :: HINQ [] (Teacher, Course) Name
query1 =
    HINQ (_select (teacherName . fst))
         (_join teachers courses teacherId teacher)
         (_where ((== "English language").(courseTitle . snd)))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName) teachers

possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 =
    HINQ (_select (teacherName . fst))
         (_join possibleTeacher possibleCourse teacherId teacher)
         (_where ((== "French language") . courseTitle . snd))

maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 =
    HINQ (_select (teacherName . fst))
         (_join possibleTeacher possibleCourse teacherId teacher)
         (_where ((== "Math") . courseTitle . snd))

data Enrollment = Enrollment
    { student :: Int
    , course :: Int } deriving Show

enrollments :: [Enrollment]
enrollments = [ Enrollment 1 101, Enrollment 2 101
              , Enrollment 2 201, Enrollment 3 101
              , Enrollment 4 201, Enrollment 4 101
              , Enrollment 5 101, Enrollment 6 201
              ]

studentsEnrollmentQ :: HINQ [] (Student, Enrollment) (Name, Int)
studentsEnrollmentQ = 
    HINQ_ (_select (\(st, en) -> (studentName st, course en)))
          (_join students enrollments studentId student)

studentsEnrollments :: [(Name, Int)]
studentsEnrollments = runHINQ studentsEnrollmentQ

englishStudentsQ :: HINQ [] ((Name, Int), Course) Name
englishStudentsQ = 
    HINQ (_select (fst . fst))
         (_join studentsEnrollments courses snd courseId)
         (_where ((== "English language") . courseTitle . snd))

englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ