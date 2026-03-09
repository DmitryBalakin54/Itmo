UPDATE Students AS S
SET Marks = (
    SELECT COUNT(*)
    FROM Marks AS M
    WHERE M.StudentId = :StudentId
)
WHERE S.StudentId = :StudentId