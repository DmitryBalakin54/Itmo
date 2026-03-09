UPDATE Marks AS M
SET Mark = COALESCE(
    (
        SELECT NM.Mark
        FROM NewMarks AS NM
        WHERE NM.StudentId = M.StudentId
        AND NM.CourseId = M.CourseId
    ),
    M.Mark
)