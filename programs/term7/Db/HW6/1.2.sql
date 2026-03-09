SELECT DISTINCT S.StudentId, S.StudentName, S.GroupId
FROM Students AS S, Groups AS G
WHERE G.GroupId = S.GroupId AND G.GroupName = :GroupName