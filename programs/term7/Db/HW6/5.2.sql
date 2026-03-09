SELECT DISTINCT Ss.StudentId
FROM Students AS Ss
WHERE NOT EXISTS (
    SELECT S.StudentId
    FROM Students AS S, Lecturers AS L, Marks AS M, Plan AS P
    WHERE S.StudentId = M.StudentId
    AND P.LecturerId = L.LecturerId
    AND P.CourseId = M.CourseId
    AND S.GroupId = P.GroupId
    AND L.LecturerName = :LecturerName
    AND S.StudentId = Ss.StudentId
)