SELECT G.GroupName, C.CourseName
FROM Groups AS G, Courses AS C
WHERE NOT EXISTS (
    SELECT Gg.GroupId, Cc.CourseId
    FROM Groups AS Gg, Courses AS Cc, Students AS S
    WHERE Gg.GroupId = G.GroupId
    AND Cc.CourseId = C.CourseId
    AND S.GroupId = Gg.GroupId
    AND NOT EXISTS (
        SELECT M.StudentId
        FROM Marks AS M
        WHERE M.StudentId = S.StudentId
        AND M.CourseId = Cc.CourseId
    )
)