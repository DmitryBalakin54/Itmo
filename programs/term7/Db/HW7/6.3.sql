UPDATE Marks AS M
SET Mark = COALESCE(
    (
        SELECT NM.Mark
        FROM NewMarks AS NM
        WHERE NM.StudentId = M.StudentId
        AND NM.CourseId = M.CourseId
        AND NM.Mark > M.Mark
    ),
    M.Mark
)