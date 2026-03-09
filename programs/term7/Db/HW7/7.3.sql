CREATE VIEW Debts(StudentId, Debts) AS
SELECT StudentId, Debts
FROM (
    SELECT StudentId, (
        SELECT COUNT(DISTINCT P.CourseId)
        FROM Plan AS P
        WHERE P.GroupId = S.GroupId
        AND P.CourseId NOT IN (
            SELECT M.CourseId
            FROM Marks AS M
            WHERE M.StudentId = S.StudentId
        )
    ) AS Debts
    FROM Students AS S
) AS D
WHERE Debts > 0