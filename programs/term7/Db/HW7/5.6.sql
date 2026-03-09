UPDATE Students AS S
SET Marks = (
    SELECT COUNT(*)
    FROM Marks AS M
    WHERE M.StudentId = S.StudentId
),
Debts = (
    SELECT COUNT(DISTINCT P.CourseId)
    FROM Plan AS P
    WHERE P.GroupId = S.GroupId
    AND P.CourseId NOT IN (
        SELECT M.CourseId
        FROM Marks AS M
        WHERE M.StudentId = S.StudentId
    ) 
)
