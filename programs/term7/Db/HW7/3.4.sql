UPDATE Students
SET StudentName = :StudentName
WHERE StudentId IN (
    SELECT StudentId
    FROM Students
    NATURAL JOIN Groups
    WHERE GroupName = :GroupName
)