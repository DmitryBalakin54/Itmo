UPDATE Students AS S
SET Marks = (
    SELECT COUNT(*)
    FROM Marks AS M
    WHERE M.StudentId = S.StudentId
)