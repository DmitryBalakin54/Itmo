UPDATE Students AS S
SET Marks = (
    SELECT COUNT(DISTINCT M.CourseId)
    FROM Marks AS M
    WHERE M.StudentId = S.StudentId
)