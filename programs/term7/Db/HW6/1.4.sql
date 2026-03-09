SELECT DISTINCT S.StudentId, S.StudentName, S.GroupId
FROM Students AS S, Marks AS M
WHERE S.StudentId = M.StudentId AND M.CourseId = :CourseId AND M.Mark = :Mark