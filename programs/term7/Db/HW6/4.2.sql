SELECT Ss.StudentName, Cc.CourseName
FROM Students AS Ss, Courses AS Cc, (
    SELECT DISTINCT S.StudentId, P.CourseId
    FROM Students AS S, Plan AS P
    WHERE S.GroupId = P.GroupId
    AND EXISTS (
        SELECT M.StudentId, M.CourseId
        FROM Marks AS M
        WHERE M.CourseId = P.CourseId
        AND M.StudentId = S.StudentId
        AND M.Mark < 3
    )
) AS V
WHERE Ss.StudentId = V.StudentId
AND Cc.CourseId = V.CourseId