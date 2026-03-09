SELECT DISTINCT S.StudentId, P.CourseId
FROM Students AS S, Plan AS P
WHERE S.GroupId = P.GroupId

UNION

SELECT DISTINCT M.StudentId, M.CourseId
FROM Marks AS M