SELECT DISTINCT S.StudentId, S.StudentName, G.GroupName
FROM Students AS S, Groups AS G
WHERE S.GroupId = G.GroupId
AND S.StudentId NOT IN (
    SELECT DISTINCT Ss.StudentId
    FROM Students AS Ss, Marks AS M
    WHERE M.StudentId = Ss.StudentId 
    AND M.CourseId = :CourseId 
)
AND S.GroupId IN (
    SELECT DISTINCT Gg.GroupId
    FROM Groups AS Gg, Plan As P
    WHERE Gg.GroupId = P.GroupId
    AND P.CourseId = :CourseId
)