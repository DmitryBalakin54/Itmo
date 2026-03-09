SELECT DISTINCT S.StudentId, S.StudentName, S.GroupId
FROM Students AS S, Marks AS M, Courses AS C
WHERE S.StudentId = M.StudentId AND M.CourseId = C.CourseId AND M.Mark = :Mark AND C.CourseName = :CourseName