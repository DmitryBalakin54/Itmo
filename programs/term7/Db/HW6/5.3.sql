SELECT DISTINCT Ss.StudentId
FROM Students AS Ss
WHERE NOT EXISTS (
    SELECT S.StudentId
    FROM Students AS S, Lecturers AS L, Plan AS P
    WHERE P.LecturerId = L.LecturerId
    AND L.LecturerName = :LecturerName
    AND S.StudentId = Ss.StudentId
    AND NOT EXISTS (
        SELECT M.StudentId
        FROM Marks AS M
        WHERE M.CourseId = P.CourseId
        AND M.StudentId = S.StudentId
    )
)