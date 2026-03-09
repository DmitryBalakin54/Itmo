SELECT DISTINCT S.StudentId, S.StudentName, G.GroupName
FROM Students AS S, Groups AS G
WHERE S.GroupId = G.GroupId
AND S.StudentId NOT IN (
    SELECT DISTINCT Ss.StudentId
    FROM Students AS Ss, Marks AS M
    WHERE M.StudentId = Ss.StudentId 
    AND M.CourseId = :CourseId 
)