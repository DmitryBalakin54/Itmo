DELETE FROM Students
WHERE StudentId IN (
    SELECT DISTINCT S.StudentId
    FROM Students AS S
    INNER JOIN PLan AS P ON P.GroupId = S.GroupId
    WHERE NOT EXISTS (
        SELECT 1
        FROM Marks AS M
        WHERE M.CourseId = P.CourseId
        AND M.StudentId = S.StudentId
    )
    GROUP BY S.StudentId
    HAVING COUNT(DISTINCT P.CourseId) >= 2
)