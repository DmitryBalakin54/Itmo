SELECT StudentName, CourseName
FROM Courses
NATURAL JOIN (
    SELECT DISTINCT StudentId, StudentName, CourseId
    FROM Students
    NATURAL JOIN Plan
) AS S
WHERE StudentId IN (
    SELECT ClubStudentHeadId FROM Clubs
)