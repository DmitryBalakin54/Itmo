UPDATE Students AS S
SET Marks = Marks + (
    SELECT COUNT(*)
    FROM NewMarks AS M
    WHERE M.StudentId = S.StudentId
)