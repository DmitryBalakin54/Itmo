DELETE FROM Students
WHERE StudentId NOT IN (
    SELECT StudentId
    FROM Marks
    GROUP BY StudentId
    HAVING COUNT(*) > 3
)