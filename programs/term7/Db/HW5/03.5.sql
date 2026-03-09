SELECT StudentId, StudentName, GroupId
FROM Students
WHERE StudentId IN (
    SELECT StudentId
    FROM Marks
    WHERE Mark = :Mark
    AND CourseId IN (
        SELECT CourseId
        FROM Plan 
        WHERE LecturerId = :LecturerId
    )
)