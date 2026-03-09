CREATE VIEW StudentMarks(StudentId, Marks) AS
SELECT StudentId, (
    SELECT COUNT(M.Mark)
    FROM Marks AS M
    WHERE M.StudentId = S.StudentId
)
FROM Students AS S