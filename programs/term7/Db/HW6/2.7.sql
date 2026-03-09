SELECT DISTINCT S.StudentId, S.StudentName, G.GroupName
FROM Students AS S, Groups AS G
WHERE S.GroupId = G.GroupId
AND S.StudentId NOT IN (
    SELECT DISTINCT Ss.StudentId
    FROM Students AS Ss, Marks AS M, Courses AS C
    WHERE M.StudentId = Ss.StudentId 
    AND M.CourseId = C.CourseId
    AND C.CourseName = :CourseName 
)
AND S.GroupId IN (
    SELECT DISTINCT Gg.GroupId
    FROM Groups AS Gg, Plan As P, Courses AS Cc
    WHERE Gg.GroupId = P.GroupId
    AND P.CourseId = Cc.CourseId
    AND Cc.CourseName = :CourseName
)