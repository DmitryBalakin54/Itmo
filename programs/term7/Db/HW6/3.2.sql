SELECT S.StudentName, C.CourseName
FROM Students AS S, Courses AS C, (
    SELECT DISTINCT Ss.StudentId, P.CourseId
    FROM Students AS Ss, Plan AS P
    WHERE Ss.GroupId = P.GroupId

    UNION

    SELECT DISTINCT M.StudentId, M.CourseId
    FROM Marks AS M
) AS V
WHERE S.StudentId = V.StudentId
AND C.CourseId = V.CourseId